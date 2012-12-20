package org.kalypso.kalypso1d2d.internal.importNet.bce2d;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypsodeegree.KalypsoDeegreePlugin;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
class Import2dData extends AbstractModelObject
{
  static final String PROPERTY_COORDINATE_SYSTEM = "coordinateSystem"; //$NON-NLS-1$

  static final String PROPERTY_IMPORT_ROUGHNESS = "importRoughness"; //$NON-NLS-1$

  private String m_coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private IScenarioDataProvider m_szenarioDataProvider;

  private final FileAndHistoryData m_inputFileData = new FileAndHistoryData( "importFile" ); //$NON-NLS-1$

  private boolean m_importRoughness = false;

  public void init( final IScenarioDataProvider szenarioDataProvider, final IDialogSettings settings )
  {
    m_szenarioDataProvider = szenarioDataProvider;

    m_inputFileData.init( settings );

    m_coordinateSystem = DialogSettingsUtils.getString( settings, PROPERTY_COORDINATE_SYSTEM, m_coordinateSystem );
    m_importRoughness = settings.getBoolean( PROPERTY_IMPORT_ROUGHNESS );
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

  public boolean getImportRoughness( )
  {
    return m_importRoughness;
  }

  public void setImportRoughness( final boolean importRoughness )
  {
    final Object oldValue = m_importRoughness;

    m_importRoughness = importRoughness;

    firePropertyChange( PROPERTY_IMPORT_ROUGHNESS, oldValue, importRoughness );
  }

  public FileAndHistoryData getInputFileData( )
  {
    return m_inputFileData;
  }

  public IScenarioDataProvider getSzenarioDataProvider( )
  {
    return m_szenarioDataProvider;
  }

  public void save( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_inputFileData.storeSettings( settings );

    settings.put( PROPERTY_COORDINATE_SYSTEM, m_coordinateSystem );
    settings.put( PROPERTY_IMPORT_ROUGHNESS, m_importRoughness );
  }
}
