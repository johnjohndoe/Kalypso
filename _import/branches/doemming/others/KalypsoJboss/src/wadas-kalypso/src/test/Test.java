package test;

import datacenter.persistent.*;
import datacenter.tree.*;
import datacenter.zeitreihen.*;


public class Test
{

	public static void main(String[] args)
	{
		// here we establish the connection with the database...
		Database.init("ca.edbc.jdbc.EdbcDriver",
		 "jdbc:edbc://PC201:TT7/BCE_PC201::dbflo/INGRES",
		 "ingres",
		 "ingres");

		//ORZeitreihenServer.init("ZeitreihenServer");

		System.out.println("started");


		//example1();


		//Channel myChannel = example2();


		//example3(myChannel);


		//example4(myChannel);


		//example5();


		example6();


   		System.out.println("exit.");
    }

	// EXAMPLE 1
	public static void example1()
	{
		/*
		 * example one: browsing through the hierarchy
		 */

		System.out.println("EXAMPLE 1");

		Level root = Level.GetRootLevel();

		printLevel(root);
	}

	// EXAMPLE 2
	public static Channel example2()
	{
		/*
		 * example two: getting a channel
		 */

		System.out.println("EXAMPLE 2");

		// let's assume that the user has selected some Container through a JTree or something like that.

		// let's say that this selected container has an id of 1
		// if you would have got the container through a call to Level.getChilds(), then a call to PLoad
		// would not be necessary
		Container selContainer = new Container(1);

		// we need to call PLoad here if we want to initialise the container from the database
		try
		{
			selContainer.PLoad();
      	}
      	catch(PersistentException e)
      	{
      		e.printStackTrace(System.out);
      		return null;
      	}

		// let's get it's channels

		Channel[] ch = selContainer.GetChannels();

		Channel myChannel = null;

		// look for a specific channel
		for(int i=0; i<ch.length; i++)
		{
			if( ch[i].GetName().equalsIgnoreCase("Kanal1"))
			{
				myChannel = ch[i];

				System.out.println("got " + myChannel);

				break;
			}
		}

		return myChannel;
	}

	// EXAMPLE 3
	public static void example3(Channel myChannel)
	{
		/*
		 * example three: working with TimeserieWrapper
		 */

		System.out.println("EXAMPLE 3");

		// get a wrapper for the original timeseries of the channel
		TimeserieWrapper wrap = myChannel.GetTSOriginal();

  		System.out.println("exporting...");

  		// export timeseries
  		wrap.ExportToFile("C:\\temp\\test.txt", null, null, null, "dd.MM.yyyy HH:mm:ss", false);

  		System.out.println("export done");

		// get a wrapper for the work timeseries
  		wrap = myChannel.GetTSWork();

  		System.out.println("importing...");

  		// import the timeseries
  		wrap.ImportFromFile("C:\\temp\\test.txt", null, null, true, "I", null, "dd.MM.yyyy HH:mm:ss", false);

  		System.out.println("import done");


  		// let's play with the computations (this is what you will probably want to use
  		// after performing your prediction computations...)
  		Computation[] comp = myChannel.GetComputations(true);

  		Computation myComp = null;

  		// look for a specific computation
  		for(int i=0; i < comp.length; i++)
  		{
  			if( comp[i].GetName().equalsIgnoreCase("testComp") )
  			{
  				myComp = comp[i];

  				System.out.println("got " + myComp);

  				break;
  			}
  		}

  		// get the wrapper for this computation
  		wrap = myComp.GetTSWrapper();

  		System.out.println("importing...");

  		// import stuff into computation
  		wrap.ImportFromFile("C:\\temp\\test.txt", null, null, true, null, null, "dd.MM.yyyy HH:mm:ss", false);

  		System.out.println("import done");
	}

	// EXAMPLE 4
	public static void example4(Channel myChannel)
	{
		/*
		 * create a computation
		 */

		System.out.println("EXAMPLE 4");

		Computation c = new Computation("testCompNew", "", myChannel.GetTSWork(), myChannel, "<prediction>");

		// synchronise with database, that is create db objet
		c.PSynchronise();

		System.out.println("created computation " + c);

		// let's imagine you perform some computation within Kalypso
		// you know want to import it in flomatis
		// say the file you generated is c:\temp\test.txt

		// first get the wrapper
		TimeserieWrapper wrap = c.GetTSWrapper();

		System.out.println("importing...");

		// use the wrapper to import
		wrap.ImportFromFile("C:\\temp\\test.txt", null, null, true, null, null, null, false);

		System.out.println("import done");
	}


	// EXAMPLE 5
	public static void example5()
	{
		/*
		 * uses fast import/export
		 */

		 System.out.println("EXAMPLE 5");

		 System.out.println("does not work right now... sorry.");

/*		 Channel c = new Channel(1);

		 try
		 {
		 	c.PLoad();

		 	System.out.println("loaded channel " + c);
		 }
		 catch(PersistentException e)
		 {
		 	e.printStackTrace(System.out);
		 }

		 TimeserieWrapper wrap = c.GetTSOriginal();

		 System.out.println("fast export...");

		 int ret = wrap.ExportToFile("J:\\wad50090\\tmp\\fastExportTest.txt", null, null, null, null, true);

		 System.out.println("done. count= " + ret);*/

		 //wrap = c.GetTSWork();

		 //System.out.println("fast import...");

		 //ret = wrap.ImportFromFile("J:\\wad50090\\tmp\\fastExportTest.txt", null, null, true, null, null, null, true);

		 //System.out.println("done. count= " + ret);
	}


	// EXAMPLE 6
	public static void example6()
	{
		/*
			create a new container
			and create a new channel within this newly created container.
		*/

		System.out.println("EXAMPLE 6");

		try
		{
			// constructor for new object
			Container cont = new Container(Level.GetRootLevel(), "new container", "");

			// always synchronise it if you want to store and later perform operations on this object
			cont.PSynchronise();

			// creates a channel object, not yet in the database
			Channel chan = new Channel(cont, "new channel", "", "xyz" + (int)Math.round(Math.random() * 100));

			// this time the channel object gets created in the database as well
			chan.PSynchronise();
		}
		catch(PersistentException e)
		{
			e.printStackTrace(System.out);
		}
	}


	// help function for printing a level
    public static void printLevel(Level level)
    {
    	System.out.println(level);

    	Level[] childs = level.GetChildLevels(false);

    	for(int i=0; i<childs.length; i++)
    	{
    		printLevel(childs[i]);
    	}

   		Container[] c = level.GetObjects(false);

		for(int j=0; j < c.length; j++)
		{
			printContainer(c[j]);
		}
    }

	// help function for printing a container
    public static void printContainer(Container obj)
    {
    	System.out.println(obj);

    	Channel[] c = obj.GetChannels();

    	for(int i=0; i< c.length; i++)
    	{
    		printChannel(c[i]);
    	}
    }

	// help function for printing a channel
	private static void printChannel(Channel channel)
	{
		System.out.println(channel);

		Computation[] c = channel.GetComputations(true);

		for(int i=0; i<c.length; i++)
		{
			printComp(c[i]);
		}
	}

	// help function for printing a computation
	private static void printComp(Computation comp)
	{
		System.out.println(comp);
	}
}
