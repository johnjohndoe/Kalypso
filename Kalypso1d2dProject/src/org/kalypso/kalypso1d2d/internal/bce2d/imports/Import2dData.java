package org.kalypso.kalypso1d2d.internal.bce2d.imports;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
class Import2dData extends AbstractModelObject
{
  private static final String SETTINGS_CRS = "crs"; //$NON-NLS-1$

  static final String PROPERTY_COORDINATE_SYSTEM = "coordinateSystem"; //$NON-NLS-1$

  private String m_coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private SzenarioDataProvider m_szenarioDataProvider;

  private final FileAndHistoryData m_inputFileData = new FileAndHistoryData( "importFile" ); //$NON-NLS-1$

  public void init( final SzenarioDataProvider szenarioDataProvider, final IDialogSettings settings )
  {
    m_szenarioDataProvider = szenarioDataProvider;

    m_inputFileData.init( settings );
    m_coordinateSystem = DialogSettingsUtils.getString( settings, SETTINGS_CRS, m_coordinateSystem );
  }

  public String getCoordinateSystem( )
  {
    return m_coordinateSystem;
  }

  public final void setCoordinateSystem( final String coordinateSystem )
  {
    final Object oldValue = m_coordinateSystem;

    m_coordinateSystem = coordinateSystem;

    firePropertyChange( PROPERTY_COORDINATE_SYSTEM, oldValue, coordinateSystem );
  }

  public FileAndHistoryData getInputFileData( )
  {
    return m_inputFileData;
  }

  public final IFEDiscretisationModel1d2d getFE1D2DDiscretisationModel( ) throws CoreException
  {
    return m_szenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName(), IFEDiscretisationModel1d2d.class );
  }

  public final IFlowRelationshipModel getFlowrelationshipModel( ) throws CoreException
  {
    return m_szenarioDataProvider.getModel( IFlowRelationshipModel.class.getName(), IFlowRelationshipModel.class );
  }

  public CommandableWorkspace getCommandableWorkspace( final String modelID ) throws Exception
  {
    return m_szenarioDataProvider.getCommandableWorkSpace( modelID );
  }

  public void postCommand( final String modelID, final ICommand command ) throws Exception
  {
    m_szenarioDataProvider.postCommand( modelID, command );
  }

  public void save( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_inputFileData.storeSettings( settings );
    settings.put( SETTINGS_CRS, m_coordinateSystem );
  }
}
