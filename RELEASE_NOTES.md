### 1.0.0-rc3 - Update mailbox and sample api
* Mailbox: Replaced createWithExpiration with create and createWithTTL
* Sample: Added function for iterative sampling

### 1.0.0-rc2 - Breaking changes to mailbox 
* Clock: Optimize and removed the excessive use of async
* Mailbox: Fix bug where future events could be sampled
* Mailbox: Fix bug where events were skipped entirely
* Mailbox: Add functionality for posting a promise
* Feed: Obsoleted take as it makes no sense a function (see debounce)
* Feed: Replaced take with debouncing (the function for removing multi events in one sample)
* Feed: Add chunkBy to group specific count of events
* Feed: Add "then" function (also known as >> in haskell)
* Feed: Add group by time
* Meta: Restructured versioning, from now this library will be SemVar compliant

### 1.0.0-rc1 - Support for longer pulses
* Added the Feed.every function

### 1.0.0-beta - Initial Release Candidate
* Support for sampling
* Support event messaging (mailbox)
* Monadic event stream (Feed)

### 0.0.1-alpha - Initial Project Setup
* Setup the project
