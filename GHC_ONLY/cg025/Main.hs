--!!! test various I/O Requests
--
--
import Trace

main  = getProgName			    exit ( \ prog ->
    	appendChan stderr (shows prog "\n") exit (

	getArgs				    exit ( \ args ->
	appendChan stderr (shows args "\n") exit (

	getEnv "PATH"			    exit ( \ path ->
	appendChan stderr (shows path "\n") exit (

	readChan stdin			    exit ( \ stdin_txt ->
	appendChan stdout stdin_txt	    exit (

	readFile (head args)		    exit ( \ file_cts ->
	appendChan stderr file_cts	    exit (

	trace "hello, trace"			 (
	getEnv "__WURBLE__" (error "hello, error\n") ( \ _ ->

	done
	))))))))))))
