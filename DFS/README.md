# Distributed file system

This project is for TCD module CS4400 taught by Stephen Barrett. The aim of this project was to build a distributed file system implementing 4 features from 7. There was the possibility to add extra features but due to time constraints I only did 4.

The features I implemented were:

* Distributed Transparent File Access
* Directory Service
* Caching
* Lock Service

More on these features below.

It is worth noting that this is one of my first Haskell programs I have build and I was certainly learning a lot as I went along and acknowledge that some aspects of my programming might not be the best. I know that if I did this in an imperative language that I was more familiar with I would have a more polished program with more features but I wanted to challenge myself and start learning functional programming and do not regret it at all. Also this is definitely the biggest project I have complete to date and under reasonable pressure with my other college and life commitments.


## Distributed Transparent File Access

This is the main functionality of any file system. There are servers holding some files which a client can access (without prior knowledge of what fileserver has the file it wants). My original intention was that I would implement the file system through restful calls, but I had issues installing modules (I was a SERIOUS Haskell newbie when I started this) so began with sockets which I intended to change to rest calls at some stage but definitely didn't have time so had to stick to my guns. In the long run it probably cost me more time using sockets but I still managed to get it working fully.

The client has simple commands

```
Open <fileName>
Close <fileName>
Write <fileName>
Kill
```
The first three are pretty self explanatory. The final one kills the client and closes the program. All the servers in this project can be shutdown with Kill (as an alternative to ctrl c).

## Directory Service

The directory service manages the mapping of files to the servers the live on. When a client sends a command to the directory service, it tells the client the port number of the server which contains the file or if the client is writing a new file, the directory service will spread the load across the file servers. This ensures that one fileserver does not get bottlenecked as it is getting all the traffic.

The directory service stores the portnumber of fileservers it knows about and a list of files stored on each fileserver. If the server was to die, it would read back in these details when it starts up.

## Caching

The client caches copy's of a file when it writes or opens (reads) it. Any open (read) of a file will first check locally to see if the file is cached before asking the directory service where the file is. This significantly the demand on the network.

When a client sends a new file (or newly accesses a file already held by the server) to the server, the server holds a record of this and if the file gets updated the server will do a "call back" to any clients who still have that file open. When I client is fully done with a file and sends a "Close" command, the fileserver removes them from its callback list. The fileserver saves all this data (as well as a record of its files) so as if the file server dies it will be able to pick back up its state when it comes back on line.

## Lock Service

The locking service this file system uses is a locking server, when a client tries to write to a file, it requests the lock from the locking server and if no other client is currently writing to that file then it receives the lock. If another file is writing to that file then the client will wait until the lock is free and then it will get the lock and be able to write through the file.

As with the other servers, the locking server will store its state incase it dies.

## Using the file system

With a terminal window in each directory or each service use

```
stack ghci <server/clientname>.hs

```

to start that service. Run all the servers before bringing a client live, and start using your distributed file system!

The client has a very basic text editor for writing to files, it supports appending or replacing file contents, but not for altering the contents that is already there.

There are two fileservers in the file system as is for testing and running the fileserver. To add another file server, you would simply have to copy one of the test fileservers, give it a unique port number, and add its name and port number to the directory servers "servers.txt" file along with a "filename.txt" file to the directory servers directory.

To add a new client, copy a test client file, give it a new port updating number and name.

## Final notes

Apart from using sockets as mentioned above, the other thing I would have liked to implement differently if I had the time would be to use a some kind of database instead of storing state in text files. Although this worked it just isn't as clean a solution as a database would have been.

Finally, I would have liked to make it easier to spin up a new client and fileserver but was unable to get to this in the time allowed but do actually intend on coming back and doing this and the database (and maybe rest) at a later date as well as dockerising it all so it could run on nodes.

Overall though, considering my knowledge of Haskell when I started I am quite happy with getting the finished version out that at least has all the core functioning of a distributed file system.


