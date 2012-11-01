package org.kalypso.kalypso1d2d.pjt.map;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

public class ExportHydrographWizardPage extends WizardPage
{
  private Button btnCheckButtonOnlySelection;

  private Button btnCheckButton;
  
  private Button btnCheckButton_1;

  private Button btnCheckButton_2;

  private Button btnCheckButton_3;

  private Button btnCheckButton_4;

  private Button btnCheckButton_5;

  private Button m_radioMultipleFiles;

  Button m_radioSingleFile;

  private Button m_btnSelectDir;
  
  private Button m_btnSeparatorButton;
  
  private Button m_btnSeparatorButton_1;

  Button m_btnHorizontal;

  Text m_sOutputDir;

  Shell m_shell;

  Text m_sSeparator;

  private IHydrograph m_selectedHydrograph;

  String m_outDefaultDir = "";//$NON-NLS-1$


  /**
   * Create the wizard.
   */
  public ExportHydrographWizardPage( final IHydrograph pSelectedHydrograph )
  {
    super( "wizardPage" );//$NON-NLS-1$
    setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.0" ) ); //$NON-NLS-1$ 
    setDescription(  Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.1" ) ); //$NON-NLS-1$ 
    m_selectedHydrograph = pSelectedHydrograph;
  }

  /**
   * Create contents of the wizard.
   * 
   * @param parent
   */
  @Override
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NONE );

    setControl( container );
    GridLayout gl_container = new GridLayout( 1, false );
    gl_container.verticalSpacing = 15;
    container.setLayout( gl_container );

    Composite composite = new Composite( container, SWT.NONE );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false, 1, 1 ) );
    composite.setLayout( new GridLayout( 2, false ) );

    Label lblNewLabel = new Label( composite, SWT.NONE );
    lblNewLabel.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.2" ) ); //$NON-NLS-1$ 
    new Label( composite, SWT.NONE );

    m_sOutputDir = new Text( composite, SWT.BORDER );
    m_sOutputDir.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 1, 1 ) );
    m_sOutputDir.setText( m_outDefaultDir );

    m_btnSelectDir = new Button( composite, SWT.NONE );
    m_btnSelectDir.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        DirectoryDialog dlg = new DirectoryDialog( m_shell );
        dlg.setFilterPath( m_sOutputDir.getText().equals( "" )? m_outDefaultDir: m_sOutputDir.getText() ); //$NON-NLS-1$ 
        dlg.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.3" ) ); //$NON-NLS-1$ 
        dlg.setMessage( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.4" ) ); //$NON-NLS-1$ 
        String dir = dlg.open();
        if( dir != null )
        {
          m_sOutputDir.setText( dir );
        }
      }
    } );
    GridData gd_m_btnSelectDir = new GridData( SWT.FILL, SWT.CENTER, false, false, 1, 1 );
    gd_m_btnSelectDir.widthHint = 90;
    m_btnSelectDir.setLayoutData( gd_m_btnSelectDir );
    m_btnSelectDir.setToolTipText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.5" ) ); //$NON-NLS-1$ 
    m_btnSelectDir.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.6" ) ); //$NON-NLS-1$ 
    
    Group grpExportOnlySelection = new Group( container, SWT.NONE );
    grpExportOnlySelection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 1, 1 ) );
    grpExportOnlySelection.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.7" ) ); //$NON-NLS-1$ 
    GridLayout gl_grpExportSelect = new GridLayout( 2, false );
    gl_grpExportSelect.marginBottom = 10;
    gl_grpExportSelect.marginRight = 10;
    gl_grpExportSelect.marginTop = 10;
    gl_grpExportSelect.marginLeft = 10;
    grpExportOnlySelection.setLayout( gl_grpExportSelect );

    btnCheckButtonOnlySelection = new Button( grpExportOnlySelection, SWT.CHECK );
    btnCheckButtonOnlySelection.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.8" ) ); //$NON-NLS-1$ 
    btnCheckButtonOnlySelection.setSelection( false );
    
    Group grpSelectFile = new Group( container, SWT.NONE );
    grpSelectFile.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.9" ) ); //$NON-NLS-1$ 
    grpSelectFile.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 1, 1 ) );
    FillLayout fl_grpSelectFile = new FillLayout( SWT.HORIZONTAL );
    fl_grpSelectFile.marginWidth = 15;
    fl_grpSelectFile.marginHeight = 10;
    grpSelectFile.setLayout( fl_grpSelectFile );

    m_radioMultipleFiles = new Button( grpSelectFile, SWT.RADIO );
    m_radioMultipleFiles.setSelection( true );
    m_radioMultipleFiles.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.10" ) ); //$NON-NLS-1$ 

    m_radioSingleFile = new Button( grpSelectFile, SWT.RADIO );
    m_radioSingleFile.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.11" ) ); //$NON-NLS-1$ 
    m_radioSingleFile.addSelectionListener( new SelectionListener()
    {
      
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_radioSingleFile.getSelection() ){
          m_btnHorizontal.setEnabled( true );
        }
        else{
          m_btnHorizontal.setEnabled( false );
        }
        
      }
      
      @Override
      public void widgetDefaultSelected( SelectionEvent e )
      {
      }
    });
    
    m_btnHorizontal = new Button( grpSelectFile, SWT.CHECK );
    m_btnHorizontal.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.12" ) );   //$NON-NLS-1$ 
    m_btnHorizontal.setSelection( true );
    m_btnHorizontal.setEnabled( false );

    Group grpSelectColumnsTo = new Group( container, SWT.NONE );
    grpSelectColumnsTo.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 1, 1 ) );
    grpSelectColumnsTo.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.13" )); //$NON-NLS-1$ 
    GridLayout gl_grpSelectColumnsTo = new GridLayout( 2, false );
    gl_grpSelectColumnsTo.marginBottom = 10;
    gl_grpSelectColumnsTo.marginRight = 10;
    gl_grpSelectColumnsTo.marginTop = 10;
    gl_grpSelectColumnsTo.marginLeft = 10;
    grpSelectColumnsTo.setLayout( gl_grpSelectColumnsTo );

    //name texts are set just for visualizing here, real values are initialized in initValues() function 
    btnCheckButton = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton.setText( "0" );  //$NON-NLS-1$
    btnCheckButton.setSelection( true );

    btnCheckButton_1 = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton_1.setText( "1" ); //$NON-NLS-1$
    btnCheckButton_1.setSelection( true );

    btnCheckButton_2 = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton_2.setText( "2" ); //$NON-NLS-1$
    btnCheckButton_2.setSelection( true );

    btnCheckButton_3 = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton_3.setText( "3" ); //$NON-NLS-1$
    btnCheckButton_3.setSelection( true );

    btnCheckButton_4 = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton_4.setText( "4" ); //$NON-NLS-1$
    btnCheckButton_4.setSelection( true );

    btnCheckButton_5 = new Button( grpSelectColumnsTo, SWT.CHECK );
    btnCheckButton_5.setText( "5" ); //$NON-NLS-1$
    btnCheckButton_5.setSelection( true );

    GridData gd_m_sSeparator = new GridData( SWT.LEFT, SWT.CENTER, false, false, 1, 1 );
    gd_m_sSeparator.widthHint = 20;
    
    Group grpSelectSeparators = new Group( container, SWT.NONE );
    grpSelectSeparators.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 1, 1 ) );
    grpSelectSeparators.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.14" ) ); //$NON-NLS-1$ 
    GridLayout gl_grpSelectDecSeparator = new GridLayout( 6, false );
    gl_grpSelectDecSeparator.marginBottom = 10;
    gl_grpSelectDecSeparator.marginRight = 10;
    gl_grpSelectDecSeparator.marginTop = 10;
    gl_grpSelectDecSeparator.marginLeft = 10;
    grpSelectSeparators.setLayout( gl_grpSelectDecSeparator );

    Label lblSeparator = new Label( grpSelectSeparators, SWT.NONE );
    lblSeparator.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, false, false, 1, 1 ) );
    lblSeparator.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.15" ) ); //$NON-NLS-1$ 

    m_sSeparator = new Text( grpSelectSeparators, SWT.BORDER );
    m_sSeparator.setLayoutData( gd_m_sSeparator );
    m_sSeparator.setSize( 20, 21 );
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

    Label lblDecEmptySeparator = new Label( grpSelectSeparators, SWT.NONE );
    lblDecEmptySeparator.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, false, false, 1, 1 ) );
    lblDecEmptySeparator.setText( "  " ); //$NON-NLS-1$
    
    Label lblDecSeparator = new Label( grpSelectSeparators, SWT.NONE );
    lblDecSeparator.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, false, false, 1, 1 ) );
    lblDecSeparator.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.18" ) ); //$NON-NLS-1$ 

    m_btnSeparatorButton = new Button( grpSelectSeparators, SWT.RADIO );
    m_btnSeparatorButton.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, false, false, 1, 1 ) );
    m_btnSeparatorButton.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.19" ) );   //$NON-NLS-1$ 
    m_btnSeparatorButton.setSelection( true );

    m_btnSeparatorButton_1 = new Button( grpSelectSeparators, SWT.RADIO );
    m_btnSeparatorButton_1.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, false, false, 1, 1 ) );
    m_btnSeparatorButton_1.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographExportWizard.20" ) );  //$NON-NLS-1$ 
    
    initValues();
  }

  private void initValues( )
  {
    final IObservation<TupleResult> o = m_selectedHydrograph.getObservation();
    final TupleResult tuples = o.getResult();

    final IComponent[] components = tuples.getComponents();

    final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    final IComponent velocityComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY );
    final IComponent dischargeComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

    final IComponent waveHsigComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
    final IComponent wavePerComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
    final IComponent waveDirComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );
    
    getBtnCheckButton().setText( waterlevelComp.getName() );
    getBtnCheckButton_1().setText( velocityComp.getName() );
    getBtnCheckButton_2().setText( dischargeComp.getName() );
    getBtnCheckButton_3().setText( waveHsigComp.getName() );
    getBtnCheckButton_4().setText( wavePerComp.getName() );
    getBtnCheckButton_5().setText( waveDirComp.getName() );    
  }

  public void setShell( final Shell pShell )
  {
    m_shell = pShell;
  }
  
  public Button getBtnCheckButton( )
  {
    return btnCheckButton;
  }

  public Button getBtnCheckButton_1( )
  {
    return btnCheckButton_1;
  }

  public Button getBtnCheckButton_2( )
  {
    return btnCheckButton_2;
  }

  public Button getBtnCheckButton_3( )
  {
    return btnCheckButton_3;
  }

  public Button getBtnCheckButton_4( )
  {
    return btnCheckButton_4;
  }

  public Button getBtnCheckButton_5( )
  {
    return btnCheckButton_5;
  }

  public Button getRadioMultipleFiles( )
  {
    return m_radioMultipleFiles;
  }

  public Button getRadioSingleFile( )
  {
    return m_radioSingleFile;
  }

  public Text getsOutputDir( )
  {
    return m_sOutputDir;
  }

  public Text getsSeparator( )
  {
    return m_sSeparator;
  }

  public Button getBtnCheckButtonOnlySelection( )
  {
    return btnCheckButtonOnlySelection;
  }

  public Button getBtnHorizontal( )
  {
    return m_btnHorizontal;
  }

  public Button getBtnSeparatorButton( )
  {
    return m_btnSeparatorButton;
  }

  public Button getBtnSeparatorButton_1( )
  {
    return m_btnSeparatorButton_1;
  }

  public void setDefaultOutDir( String outDefaultDir )
  {
    m_outDefaultDir = outDefaultDir;
  }

}
