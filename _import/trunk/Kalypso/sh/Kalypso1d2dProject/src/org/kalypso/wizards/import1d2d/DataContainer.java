package org.kalypso.wizards.import1d2d;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.command.ICommand;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class DataContainer
{
  private String m_inputFile = ""; // absolute path  //$NON-NLS-1$

  private CS_CoordinateSystem m_coordinateSystem;

  private static final CS_CoordinateSystem m_defaultCoordinateSystem = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

  private SzenarioDataProvider m_szenarioDataProvider;

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
      return new URL( "file:" + m_inputFile );  //$NON-NLS-1$
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

  public final IFEDiscretisationModel1d2d getFE1D2DDiscretisationModel( ) throws CoreException
  {
    return m_szenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class );
  }

  public void setSzenarioDataProvider( final SzenarioDataProvider szenarioDataProvider )
  {
    m_szenarioDataProvider = szenarioDataProvider;
  }

  public void postCommand( final Class< ? extends IFeatureWrapper2> clazz, final ICommand command ) throws Exception
  {
    m_szenarioDataProvider.postCommand( clazz, command );
  }
}
