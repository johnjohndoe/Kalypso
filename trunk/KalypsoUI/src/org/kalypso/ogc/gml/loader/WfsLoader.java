package org.kalypso.ogc.gml.loader;

import java.io.BufferedInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.wfs.WFSUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Kuepferle, v.Doemming
 */
public class WfsLoader extends AbstractLoader
{
  public final static String KEY_URL = "URL";

  public final static String KEY_FILTER = "FILTER";

  public final static String KEY_FEATURETYPE = "FEATURE";

  public final static String KEY_MAXFEATURE = "MAXFEATURE";

  public static final String KEY_FEATURETYPENAMESPACE = "FEATURETYPE_NAMESPACE";

  public static final int MAXFEATURE_UNBOUNDED = -1;

  /**
   * Loads a WFS DataSource from the given URL
   * 
   * @param source
   *          the href-tag from the gmt-file 'ex: http://localhost:8080/deegreewfs#river' where river denotes the
   *          feature to be loaded
   * @param context
   *          the URL form the map context (here the path to the associated gmt file)
   */
  @Override
  protected Object loadIntern( String source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    BufferedInputStream inputStream = null;
    PrintStream ps = null;
    try
    {
      monitor.beginTask( "WFS laden", 1000 );
      final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
      final String baseURLAsString = sourceProps.getProperty( KEY_URL );
      final String featureType = sourceProps.getProperty( KEY_FEATURETYPE );
      final String featureTypeNS = sourceProps.getProperty( KEY_FEATURETYPENAMESPACE );
      final String filter = sourceProps.getProperty( KEY_FILTER );
      final String maxFeature = sourceProps.getProperty( KEY_MAXFEATURE );

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      final QName qNameFT;
      if( featureTypeNS != null && featureTypeNS.length() > 0 )
        qNameFT = new QName( featureTypeNS, featureType );
      else
        qNameFT = new QName( featureType );
      final GMLWorkspace workspace = WFSUtilities.createGMLWorkspaceFromGetFeature( new URL( baseURLAsString ), qNameFT, targetCRS, filter, maxFeature );

      return new CommandableWorkspace( workspace );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( "Konnte GML von WFS nicht laden", e );
    }
    finally
    {
      monitor.done();
      IOUtils.closeQuietly( ps );
      IOUtils.closeQuietly( inputStream );
    }
  }

  public String getDescription( )
  {
    return "WFS Layer";
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  @Override
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data )
  {
    // TODO implementation of a transactional WFS
    if( data instanceof CommandableWorkspace )
    {
      Display display = new Display();
      MessageDialog md = new MessageDialog( new Shell( display ), "Speichern der Daten vom WFS", (ImageProvider.IMAGE_STYLEEDITOR_SAVE                                      ).createImage(), "Sollen die Daten Lokal gespeichrt werden?", MessageDialog.QUESTION, new String[] {
          "Ja", "Nein" }, 0 );
      int result = md.open();
      try
      {
        if( result == 0 )
        {
          IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
          SaveAsDialog dialog = new SaveAsDialog( new Shell( display ) );
          dialog.open();
          IPath targetPath = root.getLocation().append( dialog.getResult() );
          // IFile file = root.getFile( targetPath );
          FileWriter fw = new FileWriter( targetPath.toFile().toString() );
          GmlSerializer.serializeWorkspace( fw, (GMLWorkspace) data );
          ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, monitor );
        }
        else if( result == 1 )
        {

          MessageDialog.openError( new Shell( display ), "Operation not supported", "Saving a feature at a remote location is not supported" );
        }

      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      catch( GmlSerializeException e )
      {
        e.printStackTrace();
      }
      catch( CoreException e )
      {
        e.printStackTrace();
      }
      finally
      {
        display.dispose();
      }
    }
  }
}