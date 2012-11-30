package org.kalypso.kalypso1d2d.pjt.map;

import java.io.File;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.ui.ImageProvider;

public class ImportHydrographWizardPage extends WizardPage
{
  private FileChooserGroup m_shapeChooser;

  Text m_sSeparator;

  private CRSSelectionPanel m_crsPanel;

  private String m_selectedCRS;

//  private boolean m_boolValidateSeparator;

  public ImportHydrographWizardPage( final String pageName )
  {
    super( pageName );

    setImageDescriptor( ImageProvider.IMAGE_WIZBAN_IMPORT );
    setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.0" ) ); //$NON-NLS-1$
  }

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *          the parent composite
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final FileChooserDelegateOpen fileChooser = new FileChooserDelegateOpen();
    fileChooser.addFilter( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.1" ), "*.shp" ); //$NON-NLS-1$ //$NON-NLS-2$
    fileChooser.addFilter( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.2" ), "*.txt" ); //$NON-NLS-1$ //$NON-NLS-2$
    fileChooser.addFilter( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.3" ), "*.csv" ); //$NON-NLS-1$ //$NON-NLS-2$
    m_shapeChooser = new FileChooserGroup( fileChooser );
    m_shapeChooser.setDialogSettings( getDialogSettings() );
    m_shapeChooser.setLabel( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.4" ) ); //$NON-NLS-1$
    m_shapeChooser.createControlsInGrid( container );
    m_shapeChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        handleShapeFileChanged( file );
      }
    } );

    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.5" ) ); //$NON-NLS-1$

    final Composite crsContainer = new Composite( container, SWT.NONE );
    crsContainer.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    crsContainer.setLayout( new FillLayout() );
    m_crsPanel = new CRSSelectionPanel( crsContainer, CRSSelectionPanel.NO_GROUP );
    m_crsPanel.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        validatePage();
      }
    } );

    Label lblSeparator = new Label( container, SWT.NONE );
    lblSeparator.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false, 1, 1 ) );
    lblSeparator.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.15" ) ); //$NON-NLS-1$ 

    m_sSeparator = new Text( container, SWT.BORDER );
    m_sSeparator.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false, 1, 1 ) );
    m_sSeparator.setSize( 30, 21 );
    m_sSeparator.setText( "\\t" ); //$NON-NLS-1$
    final Color lBackColor = m_sSeparator.getBackground();
    final Color lRedColor =  m_sSeparator.getDisplay().getSystemColor( SWT.COLOR_RED );
    m_sSeparator.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( ModifyEvent arg0 )
      {
        if( m_sSeparator.getText().length() > 1 && !( m_sSeparator.getText().equals( "\\t" ) || m_sSeparator.getText().equals( "\\n" ) || m_sSeparator.getText().equals( "\\r" ) || m_sSeparator.getText().equals( "\\0" ) ) ){ //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
          m_sSeparator.setToolTipText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.16" ) ); //$NON-NLS-1$ 
          m_sSeparator.setBackground( lRedColor );
        }
        else{
          m_sSeparator.setToolTipText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.17" ) ); //$NON-NLS-1$ 
          m_sSeparator.setBackground( lBackColor );
        }
      }
    } );

    setPageComplete( m_shapeChooser.getFile() != null );

    setControl( container );
  }

  protected void handleShapeFileChanged( final File file )
  {
    if( file == null ){
      return;
    }
    if( file.getName() != null && ( file.getName().toLowerCase().endsWith( "csv" ) || file.getName().toLowerCase().endsWith( "txt" ) ) ){ //$NON-NLS-1$ //$NON-NLS-2$
      m_sSeparator.setEnabled( true );
//      m_boolValidateSeparator = true;
    }
    else{
      m_sSeparator.setEnabled( false );
//      m_boolValidateSeparator = false; 
    }
     
    validatePage();

    // FIXME: introduce '<none selected>' entry; page is only complete, if all entries have been selected
/*
    final IDBFField[] fields;
    if( file != null && file.exists() )
    {
      fields = readFields( file );
    }
    else
    {
      fields = new IDBFField[0];
    }

    for( final ComboViewer viewer : m_propertyViewers.values() )
    {
      viewer.getControl().setData( DATA_PROPERTY, null );
      viewer.setInput( fields );
      viewer.getControl().setEnabled( fields != null );
      if( fields.length > 0 )
        viewer.setSelection( new StructuredSelection( fields[0] ) );
    }
    */
  }
/*
  private IDBFField[] readFields( final File file )
  {
    ShapeFile shape = null;
    try
    {
      if( file == null )
        return null;

      final String shapeBase = FilenameUtils.removeExtension( file.getAbsolutePath() );
      shape = new ShapeFile( shapeBase, ShapeSerializer.getShapeDefaultCharset(), FileMode.READ );
      final IDBFField[] fields = shape.getFields();
      shape.close();
      return fields;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.4" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      setErrorMessage( msg );
      return null;
    }
    finally
    {
      try
      {
        if( shape != null )
          shape.close();
      }
      catch( final IOException ignored )
      {
        // ignored
      }
    }
  }
*/
  /**
   * Update the current page complete state based on the field content.
   */
  protected boolean validatePage( )
  {
    final IMessageProvider message = doValidate();
    boolean isValid = message == null;
    if( isValid )
      setMessage( null );
    else
      setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( isValid );
    return isValid;
  }

  private IMessageProvider doValidate( )
  {
    final IMessageProvider shapeMessage = m_shapeChooser.validate();
    if( shapeMessage != null )
      return shapeMessage;

    m_selectedCRS = m_crsPanel.getSelectedCRS();
    if( m_selectedCRS == null )
      return new MessageProvider( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.ImportWizardPage.6" ), ERROR ); //$NON-NLS-1$

    return shapeMessage;
  }

  public File getShapeFile( )
  {
    return m_shapeChooser.getFile();
  }

  public String getSelectedCRS( )
  {
    return m_selectedCRS;
  }

  public String getsSeparator( )
  {
    return m_sSeparator.getText();
  }
  
}
