package org.kalypso.wizard;

import java.io.IOException;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree_impl.io.shpapi.DBaseException;
import org.kalypsodeegree_impl.io.shpapi.HasNoDBaseFileException;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

public class CheckAutoGenerateWizardPage extends WizardPage
{

  private Composite m_topLevel;

  boolean check = false;

  String propertyName;

  Combo landusePropCombo;

  String shapeBaseFile;

  private ShapeFile shape;

  public CheckAutoGenerateWizardPage()
  {
    super( "page_type:check", "Autogenerate LanduseCollection",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    setPageComplete( true );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    initializeDialogUnits( parent );
    m_topLevel = new Composite( parent, SWT.NONE );

    GridLayout gridLayout = new GridLayout();
    m_topLevel.setLayout( gridLayout );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    m_topLevel.setLayoutData( data );

    createControlCheck( m_topLevel );
    setControl( m_topLevel );
  }

  public void createControlCheck( Composite parent )
  {

    Group group = new Group( parent, SWT.NONE );
    group.setText( "Autogenerate LanduseCollection" );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    group.setLayoutData( data );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    group.setLayout( gridLayout );

    final Button checkButton = new Button( group, SWT.CHECK );
    checkButton.setSelection( check );
    checkButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        check = checkButton.getSelection();
        if( checkButton.getSelection() )
        {
          SelectLanduseWizardPage landusePage = (SelectLanduseWizardPage)getPreviousPage();
          String currentShapeBaseFile = FileUtilities.nameWithoutExtension( landusePage
              .getLanduseDataFile().toString() );
          if( shapeBaseFile == null || !currentShapeBaseFile.equals(shapeBaseFile) )
          {
            shapeBaseFile = currentShapeBaseFile;
            loadLanduseProperties();
          }
          landusePropCombo.setEnabled( true );
          landusePropCombo.select( landusePropCombo.indexOf( propertyName ) );
        }
        else
        {
          landusePropCombo.setEnabled( false );
        }
      }
    } );

    Label landusePropLabel = new Label( group, SWT.NONE );
    landusePropLabel.setText( "Landuse PropertyName: " );

    landusePropCombo = new Combo( group, SWT.READ_ONLY );
    landusePropCombo.setEnabled(false);
    landusePropCombo.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        propertyName = landusePropCombo.getText();
      }
    } );

  }

  void loadLanduseProperties()
  {
    try
    {
      shape = new ShapeFile( shapeBaseFile );
      String[] propertyNames = shape.getProperties();
      landusePropCombo.setItems( propertyNames );
      propertyName = propertyNames[0];
      landusePropCombo.select( landusePropCombo.indexOf( propertyName ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( HasNoDBaseFileException e )
    {
      e.printStackTrace();
    }
    catch( DBaseException e )
    {
      e.printStackTrace();
    }
  }

  public boolean isCheck()
  {
    return check;
  }

  public String getPropertyName()
  {
    return propertyName;
  }

  public ShapeFile getShape()
  {
    return shape;
  }
  
  public String getShapeBaseFile()
  {
    return shapeBaseFile;
  }

  public void setShapeBaseFile( String shapeBaseFile )
  {
    this.shapeBaseFile = shapeBaseFile;
  }

}