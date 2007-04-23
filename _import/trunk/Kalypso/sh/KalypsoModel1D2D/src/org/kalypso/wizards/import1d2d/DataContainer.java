package org.kalypso.wizards.import1d2d;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class DataContainer
{

  public final static String GAUS_KRUEGER = "EPSG:31467";

  private String m_inputFile = ""; // absolute path

  private CS_CoordinateSystem m_coordinateSystem;

  private IFEDiscretisationModel1d2d m_feature;

  // TODO remove, unused
  // private IFolder m_ProjectBaseFolder;
  // private final String m_AbsolutePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();

  private static final CS_CoordinateSystem m_defaultCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( GAUS_KRUEGER );

  public DataContainer( )
  {
    super();
  }

  public final void setInputFile( String inputFile )
  {
    this.m_inputFile = inputFile;
  }

  public final void setCoordinateSystem( String coordinateSystem )
  {
    this.m_coordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( coordinateSystem );
  }

  public String getInputFile( )
  {
    return m_inputFile;
  }

  public URL getInputFileURL( )
  {
    try
    {
      return new URL( "file:" + m_inputFile ); //$NON-NLS-1$
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public CS_CoordinateSystem getCoordinateSystem( boolean getDefaultIfNull )
  {
    if( m_coordinateSystem == null && getDefaultIfNull )
      return m_defaultCoordinateSystem;
    else
      return m_coordinateSystem;
  }

  public final IFEDiscretisationModel1d2d getFE1D2DDiscretisationModel( )
  {
    return m_feature;
  }

  public final void setFE1D2DDiscretisationModel( IFEDiscretisationModel1d2d model )
  {
    m_feature = model;
  }

  // TODO remove, unused
  // public final IFolder getProjectBaseFolder( )
  // {
  // return m_ProjectBaseFolder;
  // }

  // public final void setProjectBaseFolder( IFolder currentFolder )
  // {
  // m_ProjectBaseFolder = currentFolder;
  // }
}
