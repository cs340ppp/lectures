# Asynchronous & Concurrent Programming
## CS 340: Programming Patterns and Paradigms
Michael Lee <lee@iit.edu>

## Agenda

**Lecture 1: Async Basics**
- Why Asynchronous Programming?
- Blocking vs. Non-blocking Operations
- Introduction to the Async Library
- Basic Async Operations

**Lecture 2: Async Patterns & STM**
- Concurrent Composition
- Exception Handling
- Software Transactional Memory

**Lecture 3: Concurrent Programming Patterns**
- Producer-Consumer Patterns
- Work Queues and Thread Pools
- Best Practices and Common Pitfalls

## LECTURE 1: ASYNC BASICS

## Why Asynchronous Programming?

Consider a program that needs to:
- Fetch data from multiple web services
- Process files while monitoring user input
- Handle multiple client connections

**The problem**: Traditional sequential code blocks on I/O operations, wasting
CPU time while waiting.

**The solution**: Asynchronous programming allows other work to proceed while
waiting for I/O.

## The Cost of Blocking

```haskell
-- Sequential approach: each operation waits for the previous
fetchData :: IO ()
fetchData = do
  data1 <- httpGet "http://api1.com/data"  -- blocks for 2 seconds
  data2 <- httpGet "http://api2.com/data"  -- blocks for 2 seconds
  data3 <- httpGet "http://api3.com/data"  -- blocks for 2 seconds
  process data1 data2 data3
  -- Total time: 6 seconds
```

These operations are *independent* but execute sequentially. We can do better!

## Concurrency vs. Parallelism

Before we dive in, let's clarify two related concepts:

**Concurrency**: Multiple computations making progress (possibly interleaved on
a single core)
- About *structure* and *composition*
- Example: A server handling multiple requests

**Parallelism**: Multiple computations executing simultaneously on different
cores
- About *performance* and *speedup*
- Example: Processing chunks of a large dataset

We'll focus primarily on concurrency, though the patterns overlap.

## Haskell's Concurrency Story

Haskell provides several approaches to concurrency:

1. **Lightweight threads**: `forkIO` creates cheap threads (implemented by the
   runtime)
2. **Async library**: High-level abstractions for async computations
3. **STM**: Software Transactional Memory for safe shared state
4. **Par/Strategies**: Deterministic parallelism (we won't cover these)

We'll focus on the `async` library and STM.

## The Async Library

Import with: `import Control.Concurrent.Async`

The `async` library provides a cleaner interface than raw `forkIO`:

```haskell
-- Core types
data Async a  -- A handle to an asynchronous computation

-- Key functions
async  :: IO a -> IO (Async a)
wait   :: Async a -> IO a
cancel :: Async a -> IO ()
```

An `Async a` represents a computation running in the background that will
eventually produce a value of type `a`.

## Basic Async Example

```haskell
example1 :: IO ()
example1 = do
  -- Start async computation
  a <- async $ do
    threadDelay 1000000  -- Sleep for 1 second (microseconds)
    return (2 + 2)

  putStrLn "Doing other work while computation runs..."

  -- Wait for result
  result <- wait a
  putStrLn $ "Result: " ++ show result
```

`async` starts the computation immediately. `wait` blocks until it completes.

## Live Coding: Basic Async

Let's experiment with async operations:

```haskell
-- Try these examples in GHCi

import Control.Concurrent
import Control.Concurrent.Async

-- Example: Start multiple async operations
demo1 = do
  a1 <- async $ threadDelay 1000000 >> return "First"
  a2 <- async $ threadDelay 500000 >> return "Second"
  r2 <- wait a2  -- Wait for second (faster)
  r1 <- wait a1  -- Wait for first
  print (r1, r2)
```

**Question**: Which completes first? What's the total runtime?

## Async with Real I/O

```haskell
import Network.HTTP.Simple  -- from http-conduit package

fetchUrls :: [String] -> IO [ByteString]
fetchUrls urls = do
  -- Start all requests concurrently
  asyncs <- mapM (async . httpBS . parseRequest_) urls

  -- Wait for all results
  mapM wait asyncs
```

Instead of 6 seconds sequential, we get ~2 seconds (limited by the slowest
request).

## Checking Async Status

Sometimes you want to check if a computation is done without blocking:

```haskell
poll :: Async a -> IO (Maybe (Either SomeException a))
```

Returns:
- `Nothing` if still running
- `Just (Right result)` if completed successfully
- `Just (Left exception)` if failed with exception

## Cancelling Async Operations

You can cancel an async computation if it's no longer needed:

```haskell
cancelDemo :: IO ()
cancelDemo = do
  a <- async $ do
    threadDelay 5000000
    return "Done"

  threadDelay 1000000
  cancel a  -- Cancel after 1 second

  -- This will throw an AsyncCancelled exception
  result <- wait a
  print result
```

Cancellation is cooperative—the thread receives an async exception.

## Exercise: Timeout Pattern

Implement a function that runs an IO action with a timeout:

```haskell
-- Returns Nothing if timeout expires, Just result otherwise
timeout :: Int -> IO a -> IO (Maybe a)
timeout micros action = undefined
```

Hint: Use `race` (we'll cover this next) or `async` + `cancel`.

// Solution:
// timeout micros action = do
//   a <- async action
//   threadDelay micros
//   ma <- poll a
//   case ma of
//     Nothing -> cancel a >> return Nothing
//     Just (Right result) -> return (Just result)
//     Just (Left ex) -> throwIO ex

## LECTURE 2: ASYNC PATTERNS & STM

## Concurrent Composition

The async library provides powerful combinators for composing async operations:

```haskell
-- Run two actions concurrently, return the first to complete
race :: IO a -> IO b -> IO (Either a b)

-- Run two actions concurrently, return both results
concurrently :: IO a -> IO b -> IO (a, b)

-- Wait for either of two asyncs
waitEither :: Async a -> Async b -> IO (Either a b)

-- Wait for both asyncs
waitBoth :: Async a -> Async b -> IO (a, b)
```

## Using `race`

`race` runs two computations concurrently and returns the result of whichever
finishes first, cancelling the other.

```haskell
getUserInput :: IO String
getUserInput = getLine

getDefault :: IO String
getDefault = threadDelay 5000000 >> return "default"

promptWithTimeout :: IO String
promptWithTimeout = do
  putStrLn "Enter input (5 second timeout):"
  result <- race getUserInput getDefault
  case result of
    Left input -> return input
    Right def  -> return def
```

## Using `concurrently`

`concurrently` runs two computations and waits for both to complete:

```haskell
fetchAndProcess :: IO ()
fetchAndProcess = do
  (data1, data2) <- concurrently
    (httpGet "http://api1.com/data")
    (httpGet "http://api2.com/data")

  let combined = merge data1 data2
  saveToFile combined
```

If either computation throws an exception, the other is cancelled and the
exception is re-thrown.

## Generalizing: `mapConcurrently`

For running many async operations:

```haskell
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
```

Example:

```haskell
fetchAll :: [URL] -> IO [Response]
fetchAll urls = mapConcurrently httpGet urls
```

This is like `mapM` but runs all actions concurrently. Much more efficient than
sequential `mapM` for I/O-bound operations!

## Live Coding: Concurrent Web Scraping

```haskell
-- Fetch multiple pages concurrently
scrapePages :: [String] -> IO [String]
scrapePages urls = mapConcurrently fetchPage urls
  where
    fetchPage url = do
      putStrLn $ "Fetching " ++ url
      response <- httpBS (parseRequest_ url)
      let body = decodeUtf8 $ getResponseBody response
      putStrLn $ "Completed " ++ url
      return body

-- Try with 10 URLs
main = do
  let urls = map (\n -> "http://example.com/page" ++ show n) [1..10]
  pages <- scrapePages urls
  putStrLn $ "Fetched " ++ show (length pages) ++ " pages"
```

## Exception Handling in Async Code

Exceptions in async computations are captured and re-thrown when you `wait`:

```haskell
demo :: IO ()
demo = do
  a <- async $ do
    threadDelay 1000000
    error "Something went wrong!"

  -- Exception is thrown here when we wait
  result <- wait a
  print result
```

This preserves exception safety—you can catch exceptions as usual:

```haskell
result <- (Just <$> wait a) `catch` \(_ :: SomeException) -> return Nothing
```

## Exception Handling with `concurrently`

When using `concurrently`, if either action throws an exception:

1. The exception is caught
2. The other action is cancelled
3. The exception is re-thrown

```haskell
demo :: IO ()
demo = do
  (a, b) <- concurrently
    (threadDelay 1000000 >> return "success")
    (threadDelay 500000 >> error "fail")
  -- The first action is cancelled when the second fails
  print (a, b)  -- This line never executes
```

## The Problem with Shared Mutable State

Consider this scenario with multiple threads:

```haskell
-- Thread 1                  -- Thread 2
x <- readIORef counter        x <- readIORef counter
writeIORef counter (x + 1)    writeIORef counter (x + 1)
```

**Race condition**: Both might read the same value and write the same
incremented value, losing one update!

We need a way to ensure *atomic* operations on shared state.

## Software Transactional Memory (STM)

STM provides *composable* atomic operations on shared memory.

Key idea: Operations in an STM transaction appear to execute atomically with
respect to all other transactions.

```haskell
import Control.Concurrent.STM

-- STM types
data TVar a      -- Transactional variable
type STM a       -- STM monad (transaction)

-- Core operations
newTVar   :: a -> STM (TVar a)
readTVar  :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()
atomically :: STM a -> IO a
```

## STM Example: Safe Counter

```haskell
-- Create a thread-safe counter
newCounter :: IO (TVar Int)
newCounter = atomically $ newTVar 0

-- Increment counter atomically
increment :: TVar Int -> IO ()
increment counter = atomically $ do
  count <- readTVar counter
  writeTVar counter (count + 1)

-- Use it
demo = do
  counter <- newCounter
  -- Run 1000 increments across 10 threads
  replicateM_ 10 $ async $ replicateM_ 100 $ increment counter
  threadDelay 1000000
  count <- atomically $ readTVar counter
  print count  -- Always prints 1000!
```

## STM Composability

The power of STM is *composability*. Transactions can be built from smaller
transactions:

```haskell
transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer from to amount = do
  fromBalance <- readTVar from
  toBalance <- readTVar to
  writeTVar from (fromBalance - amount)
  writeTVar to (toBalance + amount)
```

This entire operation is atomic—either both accounts update or neither does.

## STM Retry and Alternative

STM provides powerful control flow operations:

```haskell
retry :: STM a            -- Abort and retry when something changes
orElse :: STM a -> STM a -> STM a  -- Try first, if it retries try second
```

Example: Wait until account has sufficient funds:

```haskell
withdraw :: TVar Int -> Int -> STM ()
withdraw account amount = do
  balance <- readTVar account
  if balance < amount
    then retry  -- Wait until balance changes
    else writeTVar account (balance - amount)
```

## STM vs. Locks

**Traditional locks** (e.g., `MVar`, mutexes):
- Prone to deadlock
- Not composable (can't safely combine operations)
- Error-prone (forget to unlock, lock wrong order)

**STM**:
- No deadlocks (transactions automatically retry)
- Composable (combine transactions safely)
- Automatic rollback on exceptions

**Tradeoff**: STM can be slower than locks for uncontended operations, but safer
and more maintainable.

## Live Coding: Bank Account Simulation

```haskell
-- Multiple threads transferring money between accounts
type Account = TVar Int

makeAccount :: Int -> IO Account
makeAccount balance = atomically $ newTVar balance

transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
  fromBalance <- readTVar from
  when (fromBalance < amount) retry
  modifyTVar from (subtract amount)
  modifyTVar to (+ amount)

-- Simulate multiple concurrent transfers
simulate = do
  accounts <- replicateM 5 (makeAccount 1000)
  -- ... spawn threads doing random transfers
```

## Exercise: Thread-Safe Queue

Implement a thread-safe queue using STM:

```haskell
data Queue a = Queue (TVar [a]) (TVar [a])  -- front, back

newQueue :: STM (Queue a)
enqueue :: Queue a -> a -> STM ()
dequeue :: Queue a -> STM a  -- Should retry if empty
```

// Solution:
// newQueue = Queue <$> newTVar [] <*> newTVar []
//
// enqueue (Queue _ back) x = do
//   xs <- readTVar back
//   writeTVar back (x:xs)
//
// dequeue (Queue front back) = do
//   xs <- readTVar front
//   case xs of
//     (y:ys) -> writeTVar front ys >> return y
//     [] -> do
//       ys <- readTVar back
//       case reverse ys of
//         [] -> retry
//         (z:zs) -> writeTVar front zs >> writeTVar back [] >> return z

## LECTURE 3: CONCURRENT PROGRAMMING PATTERNS

## Producer-Consumer Pattern

A common concurrent pattern: producers generate data, consumers process it.

We need a *bounded buffer* between them:
- Producers block when buffer is full
- Consumers block when buffer is empty

STM makes this elegant!

## Bounded Buffer with STM

```haskell
data BoundedBuffer a = BoundedBuffer
  { capacity :: Int
  , items :: TVar [a]
  }

newBoundedBuffer :: Int -> IO (BoundedBuffer a)
newBoundedBuffer cap = atomically $
  BoundedBuffer cap <$> newTVar []

put :: BoundedBuffer a -> a -> IO ()
put buf item = atomically $ do
  xs <- readTVar (items buf)
  when (length xs >= capacity buf) retry
  writeTVar (items buf) (item : xs)

take :: BoundedBuffer a -> IO a
take buf = atomically $ do
  xs <- readTVar (items buf)
  case xs of
    [] -> retry
    (y:ys) -> writeTVar (items buf) ys >> return y
```

## Using Bounded Buffers

```haskell
-- Producer: generates numbers
producer :: BoundedBuffer Int -> IO ()
producer buf = do
  forM_ [1..100] $ \n -> do
    putStrLn $ "Producing " ++ show n
    put buf n
    threadDelay 10000  -- Simulate work

-- Consumer: processes numbers
consumer :: Int -> BoundedBuffer Int -> IO ()
consumer cid buf = forever $ do
  n <- take buf
  putStrLn $ "Consumer " ++ show cid ++ " got " ++ show n
  threadDelay 50000  -- Simulate work

-- Run with 1 producer and 3 consumers
main = do
  buf <- newBoundedBuffer 10
  async $ producer buf
  replicateConcurrently_ 3 $ \i -> consumer i buf
```

## Work Queue Pattern

Similar to producer-consumer, but tasks may produce more tasks (tree-like
computation).

Example: Web crawler
- Start with seed URLs (initial tasks)
- Fetching a page may discover new URLs (new tasks)
- Workers grab tasks from queue and process them

## Implementing a Work Queue

```haskell
data WorkQueue a = WorkQueue
  { queue :: TVar [a]
  , active :: TVar Int  -- Count of active workers
  }

newWorkQueue :: IO (WorkQueue a)
newWorkQueue = atomically $
  WorkQueue <$> newTVar [] <*> newTVar 0

addWork :: WorkQueue a -> a -> STM ()
addWork wq item = modifyTVar (queue wq) (item :)

getWork :: WorkQueue a -> STM a
getWork wq = do
  items <- readTVar (queue wq)
  case items of
    [] -> retry
    (x:xs) -> do
      writeTVar (queue wq) xs
      modifyTVar (active wq) (+1)
      return x

finishWork :: WorkQueue a -> STM ()
finishWork wq = modifyTVar (active wq) (subtract 1)
```

## Work Queue: Worker Pool

```haskell
worker :: WorkQueue Task -> (Task -> IO [Task]) -> IO ()
worker wq processTask = forever $ do
  -- Get work
  task <- atomically $ getWork wq

  -- Process it (may generate more work)
  newTasks <- processTask task

  -- Add new tasks and mark this one done
  atomically $ do
    mapM_ (addWork wq) newTasks
    finishWork wq

-- Launch worker pool
main = do
  wq <- newWorkQueue
  atomically $ addWork wq initialTask

  -- Launch 10 workers
  replicateConcurrently_ 10 $ worker wq processTask
```

## Detecting Completion

How do we know when all work is done?

Work is complete when:
1. The queue is empty, AND
2. No workers are active

```haskell
isDone :: WorkQueue a -> STM Bool
isDone wq = do
  items <- readTVar (queue wq)
  activeCount <- readTVar (active wq)
  return (null items && activeCount == 0)

waitForCompletion :: WorkQueue a -> IO ()
waitForCompletion wq = atomically $ do
  done <- isDone wq
  unless done retry
```

## Live Coding: Web Crawler

```haskell
type URL = String

data CrawlTask = CrawlTask
  { url :: URL
  , depth :: Int
  }

crawl :: WorkQueue CrawlTask -> CrawlTask -> IO [CrawlTask]
crawl wq task = do
  when (depth task > 3) $ return []  -- Max depth

  putStrLn $ "Crawling " ++ url task
  page <- httpGet (url task)
  let newUrls = extractLinks page

  return [CrawlTask u (depth task + 1) | u <- newUrls]

main = do
  wq <- newWorkQueue
  atomically $ addWork wq (CrawlTask "http://start.com" 0)

  replicateConcurrently_ 5 $ \_ -> worker wq (crawl wq)
  waitForCompletion wq
  putStrLn "Crawling complete!"
```

## Async Best Practices

1. **Prefer `async` over `forkIO`**: Better exception handling and resource
   management

2. **Use `bracket` patterns**: Ensure cleanup even if exceptions occur
   ```haskell
   withAsync action $ \a -> do
     -- a is automatically cancelled when this block exits
     result <- wait a
     return result
   ```

3. **Be careful with `cancel`**: Cancellation is asynchronous; wait if you need
   to ensure it's done

4. **Use STM for shared state**: More maintainable than low-level primitives

5. **Bound your concurrency**: Don't spawn unbounded threads; use worker pools

## Common Pitfalls

1. **Forgetting to `wait`**: Async computations are cancelled when garbage
   collected

2. **Space leaks with `concurrently`**: Both results are kept in memory even if
   only one is needed

3. **STM transactions too large**: Keep transactions short; I/O in STM blocks
   retrying

4. **Neglecting exceptions**: Async code can hide exceptions; always handle them

5. **Race conditions in logic**: Concurrency changes timing; test thoroughly!

## Debugging Concurrent Code

Debugging concurrent programs is hard. Some techniques:

1. **ThreadScope**: Visualize thread activity
   - Compile with `-threaded -rtsopts -eventlog`
   - Run with `+RTS -N -ls`
   - View with `threadscope`

2. **Logging**: Add strategic print statements with thread IDs

3. **Property-based testing**: Use QuickCheck to test concurrent invariants

4. **Simplify first**: Test with single thread, then increase concurrency

## Exercise: Parallel Map-Reduce

Implement a parallel map-reduce using async:

```haskell
-- Split input into chunks, map over chunks concurrently, reduce results
parMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> IO c
parMapReduce mapFn reduceFn items = undefined
```

Hints:
- Use `chunksOf` to split the list
- Use `mapConcurrently` to process chunks
- Apply `reduceFn` to combined results

// Solution:
// parMapReduce mapFn reduceFn items = do
//   let chunks = chunksOf chunkSize items
//       chunkSize = max 1 (length items `div` numCapabilities)
//   results <- mapConcurrently (return . map mapFn) chunks
//   return $ reduceFn (concat results)

## Async vs. Par/Strategies

We focused on `async` for I/O concurrency. For CPU-bound parallelism, consider:

**Par monad** (`Control.Monad.Par`):
- Deterministic parallelism
- Pure computations only
- Explicit data dependencies

**Evaluation Strategies** (`Control.Parallel.Strategies`):
- Separates algorithm from parallelism
- `parMap`, `parList`, etc.
- Works with lazy evaluation

Both offer more control for pure parallel computations, but `async` is more
flexible for I/O.

## When to Use Concurrency

Use concurrency when:
- Performing multiple I/O operations
- Building servers or event-driven systems
- Coordinating independent tasks
- Improving responsiveness (background tasks)

Don't use concurrency when:
- Sequential code is simpler and fast enough
- Overhead exceeds benefits
- Correctness is too hard to ensure

**Remember**: Concurrency is a tool, not a goal. Profile before optimizing!

## Cross-Language Comparison: Python

Python's async story (asyncio):

```python
async def fetch_urls(urls):
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_one(session, url) for url in urls]
        return await asyncio.gather(*tasks)

async def fetch_one(session, url):
    async with session.get(url) as response:
        return await response.text()
```

- Uses `async`/`await` syntax
- Single-threaded event loop (GIL limits true parallelism)
- More manual management of async context

## Cross-Language Comparison: Rust

Rust's async story (tokio):

```rust
async fn fetch_urls(urls: Vec<String>) -> Vec<String> {
    let client = reqwest::Client::new();
    let tasks: Vec<_> = urls.iter()
        .map(|url| client.get(url).send())
        .collect();

    let responses = futures::future::join_all(tasks).await;
    // ... process responses
}
```

- Zero-cost abstractions
- Compile-time async runtime selection
- Similar compositional style to Haskell

## Summary: Async/Concurrency

**Async library**: High-level primitives for concurrent I/O
- `async`, `wait`, `cancel`
- `race`, `concurrently`, `mapConcurrently`

**STM**: Composable atomic transactions
- `TVar`, `atomically`, `retry`, `orElse`
- Eliminates many concurrency bugs

**Patterns**: Producer-consumer, work queues, parallel composition

**Best practices**: Use async over forkIO, bound concurrency, handle exceptions

These tools let you write concurrent programs that are both efficient and
maintainable!
