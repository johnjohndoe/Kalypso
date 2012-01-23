package org.kalypso.wizards.import1d2d;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.model.IModel;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.preferences.IKalypsoDeegreePreferences;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class DataContainer
{
  private String m_inputFile = ""; // absolute path //$NON-NLS-1$

  private String m_coordinateSystem;

  private static final String m_defaultCoordinateSystem = IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE;

  private SzenarioDataProvider m_szenarioDataProvider;

  public final void setInputFile( final String inputFile )
  {
    this.m_inputFile = inputFile;
  }

  public final void setCoordinateSystem( final String coordinateSystem )
  {
    this.m_coordinateSystem = coordinateSystem;
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
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @deprecated Very suspicious. Fetch the kalypso coordinate system instead. Why is there a coordinate system variable
   *             here at all?
   */
  @Deprecated
  public String getCoordinateSystem( final boolean getDefaultIfNull )
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

  public void postCommand( final Class< ? extends IModel> clazz, final ICommand command ) throws Exception
  {
    m_szenarioDataProvider.postCommand( clazz, command );
  }
}
