/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.interpolation.wizard;

import java.io.File;
import java.io.FileReader;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypso.ogc.gml.filterdialog.model.FilterRootElement;
import org.kalypso.transformation.CRSHelper;

/**
 * @author kuepfer
 */
public class GridImportWizardPage extends WizardPage implements ModifyListener, SelectionListener, KeyListener
{
  private Composite m_topComposite;

  private Text m_sourceFileText;

  private Group m_group;

  private static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Button m_browseButton;

  private Combo m_checkCRS;

  private Mesh m_mesh;

  private double m_size;

  private IPath m_path;

  public GridImportWizardPage( String pageName )
  {
    super( pageName );
    setDescription( "Interpolation eines Netzes auf einen definiertes Raster." );
    setPageComplete( false );
  }

  public GridImportWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topComposite = new Composite( parent, SWT.NULL );
    m_topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );
    m_topComposite.setLayout( new GridLayout() );
    m_topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // build wizard page
    createGroup( m_topComposite );

    setControl( m_topComposite );
  }

  private void createGroup( Composite parent )
  {
    m_group = new Group( parent, SWT.NULL );
    GridLayout topGroupLayout = new GridLayout();
    GridData topGroupData = new GridData();
    topGroupLayout.numColumns = 3;
    topGroupData.horizontalAlignment = GridData.FILL;
    m_group.setLayout( topGroupLayout );
    m_group.setLayoutData( topGroupData );
    m_group.setText( "Filter-Datei" );
    Label sourceFileLabel = new Label( m_group, SWT.NONE );
    sourceFileLabel.setText( "Quelle : " );

    // Set width of source path field
    GridData data0 = new GridData( GridData.FILL_HORIZONTAL );
    data0.widthHint = SIZING_TEXT_FIELD_WIDTH;

    m_sourceFileText = new Text( m_group, SWT.BORDER );
    m_sourceFileText.setLayoutData( data0 );
    m_sourceFileText.setEditable( false );
    m_sourceFileText.addModifyListener( this );

    m_browseButton = new Button( m_group, SWT.PUSH );
    m_browseButton.setText( "Durchsuchen..." );
    m_browseButton.setLayoutData( new GridData( GridData.END ) );
    m_browseButton.addSelectionListener( this );

    Label crsLabel = new Label( m_group, SWT.NONE );
    crsLabel.setText( "Koordinaten System: " );

    m_checkCRS = new Combo( m_group, SWT.NONE );

    availableCoordinateSystems( m_checkCRS );

    String defaultCS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    m_checkCRS.select( m_checkCRS.indexOf( defaultCS ) );

    m_checkCRS.setToolTipText( "Koordinatensystem der ESRI(tm) Shape Datei" );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_checkCRS.setLayoutData( data );
    m_checkCRS.addSelectionListener( this );
    m_checkCRS.addKeyListener( this );
    m_group.pack();

  }

  private void availableCoordinateSystems( Combo checkCRS )
  {
    checkCRS.setItems( CRSHelper.getAllNames().toArray( new String[] {} ) );
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    Widget widget = e.widget;
    if( widget instanceof Button )
    {
      FileDialog dialog = new FileDialog( getShell() );
      String open = dialog.open();
      FilterRootElement root = null;
      try
      {
        if( open != null )
        {
          FileReader reader = new FileReader( open );
          // m_root = FilterReader.collectFiltersFromSLD( reader );
        }
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    // TODO Auto-generated method stub
  }

  public Mesh getMesh( )
  {
    return m_mesh;
  }

  public String getCoordinateSystem( )
  {
    return null;
  }

  public double getCellSize( )
  {
    return m_size;
  }

  public File getTargetFile( )
  {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    root.getFile( m_path );
    return null;
  }
}
