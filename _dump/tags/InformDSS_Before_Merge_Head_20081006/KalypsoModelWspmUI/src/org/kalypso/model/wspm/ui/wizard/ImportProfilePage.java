/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/*
 * Created on 31.01.2005
 *  
 */
package org.kalypso.model.wspm.ui.wizard;

import java.io.File;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.PlatformUI;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.transformation.ui.CRSSelectionListener;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Thomas Jung
 */
public class ImportProfilePage extends WizardPage implements SelectionListener, ModifyListener, KeyListener
{

  // constants
  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private static final String TAB = "<TAB>"; //$NON-NLS-1$

  private static final String[] SEPARATOR = new String[] { ",", ";", TAB }; //$NON-NLS-1$ //$NON-NLS-2$

  // widgets
  private Group m_group;

  private Label m_sourceFileLabel;

  private Text m_sourceFileText;

  private Composite m_topComposite;

  private Button m_browseButton;

  private String m_fileName;

  private File m_file;

  private String m_filePath;

  protected String m_crs;

  private Combo m_separatorCombo;

  private String m_seperator;

  private CRSSelectionPanel m_crsPanel;

  /**
   * @param pageName
   */
  public ImportProfilePage( String pageName )
  {
    super( pageName );
    setDescription( "Dieser Dialog liest eine ASCII-Trippel-Datei in den Workspace ein" ); //$NON-NLS-1$
    setPageComplete( false );
  }

  /**
   * @param pageName
   * @param title
   * @param titleImage
   */
  public ImportProfilePage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setDescription( "Dieser Dialog liest eine ASCII-Trippel-Datei in den Workspace ein" ); //$NON-NLS-1$
    setPageComplete( false );
  }

  /*
   * (non-Javadoc)
   * 
   * @see wizard.eclipse.jface.dialogs.IDialogPage#createControl(wizard.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    // build wizard page
    // TODO: some nice text...
    createFileGroup( m_topComposite );

    m_crsPanel = new CRSSelectionPanel( m_topComposite, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_crsPanel.setToolTipText( Messages.ImportProfilePage_3 );

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      @Override
      protected void selectionChanged( String selectedCRS )
      {
        m_crs = selectedCRS;
        validate();
      }

    } );

    final Label seperatorLabel = new Label( m_group, SWT.NONE );
    seperatorLabel.setText( Messages.ImportProfilePage_4 );

    m_separatorCombo = new Combo( m_group, SWT.READ_ONLY );
    final GridData sepData = new GridData( SWT.FILL, SWT.FILL, false, false );

    m_separatorCombo.setToolTipText( Messages.ImportProfilePage_5 );
    m_separatorCombo.setLayoutData( sepData );
    m_separatorCombo.addSelectionListener( this );
    m_separatorCombo.addKeyListener( this );

    m_separatorCombo.setItems( SEPARATOR );
    m_separatorCombo.select( 0 );

    /* Dummy label. */
    new Label( m_group, SWT.NONE );

    setControl( m_topComposite );
  }

  private void createFileGroup( Composite parent )
  {
    m_group = new Group( parent, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( "Profil-Datei" ); //$NON-NLS-1$
    m_sourceFileLabel = new Label( m_group, SWT.NONE );
    m_sourceFileLabel.setText( "Quelle: " ); //$NON-NLS-1$

    // Set width of source path field
    GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );
    m_sourceFileText.addModifyListener( this );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." ); //$NON-NLS-1$
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );

    m_group.pack();

  }

  void validate( )
  {
    setErrorMessage( null );
    boolean pageComplete = true;

    // File
    if( m_sourceFileText.getText() != null && m_sourceFileText.getText().length() > 0 )
    {
      // ok
    }
    else
    {
      setErrorMessage( "Bitte eine Profil-Datei auswählen!" ); //$NON-NLS-1$
      pageComplete = false;
    }

    // CoordinateSystem
    if( m_crs == null || !CRSHelper.isKnownCRS( m_crs ) )
    {
      setErrorMessage( "Gewähltes KoordinatenSystem wird nicht unterstützt!" ); //$NON-NLS-1$
      pageComplete = false;
    }

    // separator
    final String sepText = m_separatorCombo.getText();
    if( sepText == null )
      pageComplete = false;
    else
    {
      if( sepText.equals( TAB ) )
        m_seperator = "\t"; //$NON-NLS-1$
      else
        m_seperator = sepText;

      setPageComplete( pageComplete );
    }
  }

  // SelectionListener
  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    Button b;
    Combo d;
    if( e.widget instanceof Button )
    {
      b = (Button) e.widget;
      if( b.equals( m_browseButton ) )
      {
        Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
        final FileDialog dialog = new FileDialog( shell, SWT.OPEN );
        dialog.setText( "Trippel-Import" ); //$NON-NLS-1$
        dialog.setFilterExtensions( new String[] { "*.txt", "*.*" } ); //$NON-NLS-1$ //$NON-NLS-2$
        dialog.open();

        final String filterPath = dialog.getFilterPath();

        final String fileName = dialog.getFileName();
        final String name = fileName;
        File result = new File( filterPath, name );

        if( fileName != null )
        {
          m_sourceFileText.setText( fileName.toString() );
          m_fileName = fileName;
          m_file = result;
          m_filePath = filterPath;
        }
      }
      if( e.widget instanceof Combo )
      {
        if( e.widget == m_separatorCombo )
        {
          d = (Combo) e.widget;
          if( d == m_separatorCombo )
          {
            setPageComplete( true );
          }
        }
      }
    }
    validate();
  }

  public File getFile( )
  {
    return m_file;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public String getFilePath( )
  {
    return m_filePath;
  }

  public String getSeparator( )
  {
    return m_seperator;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    // no default selection
  }

  // ModifyListener
  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    validate();
  }

  // KeyListener
  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    Widget w = e.widget;
    if( w instanceof Combo && e.character == SWT.CR )
    {
      validate();
    }
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    // do nothing
  }

  public void removeListeners( )
  {
    m_browseButton.removeSelectionListener( this );
  }

  public String getCoordinateSystem( )
  {
    return m_crs;
  }
}
