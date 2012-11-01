package org.kalypso.kalypso1d2d.internal.import2d;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.kalypso1d2d.internal.I2DContants;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Import2dPage extends WizardPage
{
  private final Import2dData m_data;

  private CRSSelectionPanel m_crsPanel;

  private String m_crs;

  private DatabindingWizardPage m_binding;

  public Import2dPage( final Import2dData data )
  {
    this( "mainPage", data ); //$NON-NLS-1$
  }

  protected Import2dPage( final String name, final Import2dData data )
  {
    super( name );

    m_data = data;

    setTitle( Messages.getString( "org.kalypso.wizards.import1d2d.PageMain.1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.wizards.import1d2d.PageMain.2" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    // create the composite to hold the widgets
    final Composite composite = new Composite( parent, SWT.NULL );
    setControl( composite );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( composite );

    m_binding = new DatabindingWizardPage( this, null );

    createImportFileControls( composite );
    createSrsControls( composite );
    createImportRoughnessControls( composite );

    /* Initially clear message */
    setErrorMessage( null );
  }

  private void createImportFileControls( final Composite composite )
  {
    new Label( composite, SWT.NONE ).setText( Messages.getString( "org.kalypso.wizards.import1d2d.PageMain.5" ) ); //$NON-NLS-1$

    final FileChooserDelegateOpen chooserDelegate = new FileChooserDelegateOpen();
    chooserDelegate.addFilter( I2DContants.STR_FILTERNAME_2D, I2DContants.EXTENSION_2D );

    final FileAndHistoryData inputFileData = m_data.getInputFileData();

    final IObservableValue fileTarget = BeansObservables.observeValue( inputFileData, FileAndHistoryData.PROPERTY_FILE );
    final IObservableValue historyTarget = BeansObservables.observeValue( inputFileData, FileAndHistoryData.PROPERTY_HISTORY );

    final FileBinding fileBinding = new FileBinding( m_binding, fileTarget, chooserDelegate );
    final Control fileControl = fileBinding.createFileFieldWithHistory( composite, historyTarget );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    fileBinding.createFileSearchButton( composite, fileControl );
  }

  private void createSrsControls( final Composite parent )
  {
    m_crsPanel = new CRSSelectionPanel( parent, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 3, 1 ) );

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.wizards.import1d2d.PageMain.7" ) ); //$NON-NLS-1$

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );

    final IObservableValue crsTarget = m_crsPanel.observe();
    final IObservableValue crsModel = BeansObservables.observeValue( m_data, Import2dData.PROPERTY_COORDINATE_SYSTEM );
    m_binding.bindValue( crsTarget, crsModel );
  }

  private void createImportRoughnessControls( final Composite parent )
  {
    final Button checkbox = new Button( parent, SWT.CHECK );
    checkbox.setText( Messages.getString("Import2dPage.0") ); //$NON-NLS-1$
    checkbox.setToolTipText( Messages.getString("Import2dPage.1") ); //$NON-NLS-1$
    checkbox.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );

    final ISWTObservableValue target = SWTObservables.observeSelection( checkbox );
    final IObservableValue model = BeansObservables.observeValue( m_data, Import2dData.PROPERTY_IMPORT_ROUGHNESS );
    m_binding.bindValue( target, model );
  }
}