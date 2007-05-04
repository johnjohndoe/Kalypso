package de.renew.workflow.connector;

import java.io.File;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import de.renew.plugin.PluginManager;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowServer
{
  private static final String DE_RENEW_NET_PATH_PROPERTY = "de.renew.netPath";

  private static final Logger logger = Logger.getLogger( WorkflowServer.class.getName() );

  public static void main( final String[] args )
  {
    final WorkflowServer workflowServer = new WorkflowServer();
    workflowServer.start( args.length > 0 ? true : false );
  }

  public void start( final boolean gui )
  {
    try
    {
      setNetPath();
      final String[] args = gui ? getGuiArgs() : getShadowSystemArgs();
      startRenew( args );
    }
    catch( final Exception e )
    {
      // so much could happen...
      logger.log( Level.SEVERE, "Problem starting Renew", e );
    }
  }

  private String[] getShadowSystemArgs( )
  {
    return new String[] { "startsimulation", System.getProperty( DE_RENEW_NET_PATH_PROPERTY ) + "/ProjectManagement.sns", "ProjectManagement" };
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
    final String netSourceDir = ".";
    logger.info( "netPath: " + netSourceDir );
    System.setProperty( DE_RENEW_NET_PATH_PROPERTY, netSourceDir );
  }

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

  private void startRenew( final String[] args )
  {
    PluginManager.main( args );
  }
}
