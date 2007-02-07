package de.renew.workflow;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Vector;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import de.renew.plugin.PluginManager;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowServer
{
  private static final String DE_RENEW_SIMDB_PASSWORD_PROPERTY = "de.renew.simdb.password";

  private static final String DE_RENEW_SIMDB_PASSWORD = "";

  private static final String DE_RENEW_SIMDB_USER_PROPERTY = "de.renew.simdb.user";

  private static final String DE_RENEW_SIMDB_USER = "sa";

  private static final String JAVA_RMI_SERVER_IGNORE_STUB_CLASSES_PROPERTY = "java.rmi.server.ignoreStubClasses";

  private static final String JAVA_RMI_SERVER_IGNORE_STUB_CLASSES = "true";

  private static final String DE_RENEW_SIMDB_URL_PROPERTY = "de.renew.simdb.url";

  private static final String SIM_DB_URL_PREFIX = "jdbc:hsqldb:file:";

  private static final String JAVA_RMI_SERVER_CODEBASE_PROPERTY = "java.rmi.server.codebase";

  private static final String DE_RENEW_NET_PATH_PROPERTY = "de.renew.netPath";

  private static final Logger logger = Logger.getLogger( WorkflowServer.class );

  private static final boolean gui = true;

  // private static final boolean log = true;

  // static
  // {
  // if( !log )
  // logger.setUseParentHandlers( false );
  // }

  static
  {
    BasicConfigurator.configure();
  }

  public static void main( String[] args )
  {
    final WorkflowServer workflowServer = new WorkflowServer();
    workflowServer.start();
  }

  public void start( )
  {
    try
    {
      final String dbDir = "model1D2Ddb/model1D2D";
      setSzenarioFolder( dbDir );
      setNetPath();
      setCodebase();

      System.setProperty( JAVA_RMI_SERVER_IGNORE_STUB_CLASSES_PROPERTY, JAVA_RMI_SERVER_IGNORE_STUB_CLASSES );
      System.setProperty( DE_RENEW_SIMDB_USER_PROPERTY, DE_RENEW_SIMDB_USER );
      System.setProperty( DE_RENEW_SIMDB_PASSWORD_PROPERTY, DE_RENEW_SIMDB_PASSWORD );

      // setupDB();

      final String[] args = gui ? getGuiArgs() : getShadowSystemArgs();
      startRenew( args );
    }
    catch( final Exception e )
    {
      // so much could happen...
      logger.error( "Problem starting Renew", e );
    }
  }

  private String[] getShadowSystemArgs( )
  {
    return new String[] { "startsimulation", System.getProperty( DE_RENEW_NET_PATH_PROPERTY ) + "/WF_Kalypso1D2D.sns", "WF_Kalypso1D2D" };
  }

  private String[] getGuiArgs( )
  {
    final Vector<String> renewArgs = new Vector<String>();
    renewArgs.add( "gui" );
    for( final File net : getFiles( new File( System.getProperty( DE_RENEW_NET_PATH_PROPERTY ) ), ".rnw" ) )
    {
      renewArgs.add( net.toURI().toString() );
    }
    return renewArgs.toArray( new String[renewArgs.size()] );
  }

  private void setNetPath( )
  {
    // final String netSourceDir = WorkflowConnectorPlugin.getDefault().getBundle().getLocation() +
    // "src/model1D2Dnetsrc";
    final String netSourceDir = "model1D2Dnetsrc";
    logger.info( "netPath: " + netSourceDir );
    System.setProperty( DE_RENEW_NET_PATH_PROPERTY, netSourceDir );
  }

  private void setSzenarioFolder( final String dbDir )
  {
    logger.info( "szenarioFolder: " + dbDir );
    System.setProperty( DE_RENEW_SIMDB_URL_PROPERTY, SIM_DB_URL_PREFIX + dbDir );
  }

  // private static void copyFile( final File file, final File ziel ) throws FileNotFoundException, IOException
  // {
  // final BufferedInputStream in = new BufferedInputStream( new FileInputStream( file ) );
  // final BufferedOutputStream out = new BufferedOutputStream( new FileOutputStream( ziel, true ) );
  // int bytes = 0;
  // while( (bytes = in.read()) != -1 )
  // { // Datei einlesen
  // out.write( bytes ); // Datei schreiben
  // }
  // in.close();
  // out.close();
  // logger.info( "copied " + file.getName() + " to " + ziel.getAbsolutePath() );
  // }

  // private static void copyDir( final File quelle, final File ziel ) throws FileNotFoundException, IOException
  // {
  // final File[] files = quelle.listFiles();
  // File newFile = null;
  // ziel.mkdirs(); // erstellt alle benötigten Ordner
  // if( files != null )
  // {
  // for( int i = 0; i < files.length; i++ )
  // {
  // newFile = new File( ziel.getAbsolutePath() + System.getProperty( "file.separator" ) + files[i].getName() );
  // if( files[i].isDirectory() )
  // {
  // copyDir( files[i], newFile );
  // }
  // else
  // {
  // copyFile( files[i], newFile );
  // }
  // }
  // }
  // }

  private static Vector<File> getFiles( final File dir, final String ending )
  {
    final Vector<File> v = new Vector<File>();

    final File[] files = dir.listFiles();
    for( int x = 0; x < files.length; x++ )
    {
      if( files[x].isFile() )
      {
        if( files[x].getName().endsWith( ending ) )
        {
          v.add( files[x] );
        }
      }
      else if( files[x].isDirectory() )
      {
        v.addAll( getFiles( files[x], ending ) );
      }
    }

    return v;
  }

  private void setCodebase( ) throws IOException, Exception, URISyntaxException
  {
    // final String libDir = WorkflowConnectorPlugin.getDefault().getBundle().getLocation() + "lib";
    final String libDir = "lib";
    logger.info( "codebase: " + libDir );
    final StringBuffer jarsString = new StringBuffer( "file:/./ " );
    for( final File jar : getFiles( new File( libDir ), ".jar" ) )
    {
      jarsString.append( jar.toURI().toString() );
      jarsString.append( ' ' );
    }
    System.setProperty( JAVA_RMI_SERVER_CODEBASE_PROPERTY, jarsString.toString() );
  }

  // private static void setupDB( ) throws FileNotFoundException, IOException
  // {
  // final String userHomeString = System.getProperty( "user.home" );
  // final File userHome = new File( userHomeString );
  // final File dbDefaults = new File( DB_DEFAULTS_DIRECTORY );
  // logger.info( "Copying " + dbDefaults.getAbsolutePath() + " to " + userHome.getAbsolutePath() );
  // copyDir( dbDefaults, new File( userHome, DB_DIRECTORY ) );
  // }

  private void startRenew( final String[] args )
  {
    // try
    // {
    // final Class loader = WorkflowConnectorPlugin.getDefault().getBundle().loadClass( "de.renew.plugin.Loader" );
    // final URL location = loader.getProtectionDomain().getCodeSource().getLocation();
    // logger.info( "Loader location: " + location );
    // PluginManager.main( netFiles, location );
    // }
    // catch( final ClassNotFoundException e )
    // {
    // logger.error( "Renew Loader could not be found", e );
    // }
    PluginManager.main( args );
    // final CPNDrawingLoader drawingLoader = new CPNDrawingLoader();
    // final SimulatorPlugin simulatorPlugin = SimulatorPlugin.getCurrent();
    // simulatorPlugin.registerDefaultNetFinder( new GuiFinder( drawingLoader ) );
    // final CPNSimulation simulation = new CPNSimulation( false, drawingLoader );
    // simulation.simulationRun();

    // simulatorPlugin.addExtension( new SimulatorExtensionAdapter()
    // {
    // /**
    // * @see de.renew.application.SimulatorExtensionAdapter#simulationSetup(de.renew.application.SimulationEnvironment)
    // */
    // @Override
    // public void simulationSetup( SimulationEnvironment env )
    // {
    // simulation.simulationRun();
    // simulatorPlugin.removeExtension( this );
    // }
    // } );

    // try {
    // PluginManager.main(new String[] { "startsimulation",
    // NETSRC + "/WF_Kalypso1D2D.sns", NETSRC + "/WF_Kalypso1D2D",
    // "-i" }, new File("lib/renew-2.2dev").toURL());
    // } catch (MalformedURLException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // }
  }
}
