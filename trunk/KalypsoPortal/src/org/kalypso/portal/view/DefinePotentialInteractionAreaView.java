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
package org.kalypso.portal.view;

import java.util.Hashtable;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author kuepfer
 */
public class DefinePotentialInteractionAreaView extends ViewPart
{

  Hashtable<IKalypsoTheme, Integer> m_hash = new Hashtable<IKalypsoTheme, Integer>();

  private TabFolder m_tabFolder;

  public DefinePotentialInteractionAreaView( )
  {
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    Composite top = new Composite( parent, SWT.NONE );
    top.setLayout( new GridLayout( 1, false ) );
    top.setLayoutData( new GridData() );
    createTabFolder( top );

  }

  private void createTabFolder( Composite top )
  {
    Group confGroup = new Group( top, SWT.NONE );
    confGroup.setText( "Betroffenheiten im Plangebiet" );
    IEditorPart activeEditor = getSite().getPage().getActiveEditor();
    m_tabFolder = new TabFolder( confGroup, SWT.NULL );
//    if( activeEditor instanceof GisMapEditor )
//    {
//      final GisMapEditor gisMapEditor = (GisMapEditor) activeEditor;
//      final MapPanel mapPanel = gisMapEditor.getMapPanel();
//      final IMapModell mapModell = mapPanel.getMapModell();
//      final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
//      for( int i = 0; i < allThemes.length; i++ )
//      {
//        final IKalypsoTheme theme = allThemes[i];
//        final String name = theme.getName();
//        createTabItem( m_tabFolder, name, null );
//      }
//    }

  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    // TODO Auto-generated method stub

  }

  protected TabItem createTabItem( final TabFolder parent, final String text, final Image icon )
  {
    return createTabItem( parent, SWT.NONE, text, icon );
  }

  protected TabItem createTabItem( final TabFolder parent, int style, final String text, final Image icon )
  {
    TabItem ti = new TabItem( parent, style );
    if( text != null )
      ti.setText( text );
    else
      ti.setText( "Default Text" );
    if( icon != null )
      ti.setImage( icon );
    // create tab control
    Composite top = new Composite( parent, SWT.NULL );
    top.setLayout( new GridLayout(2, false ) );
    top.setLayoutData( new GridData() );
    Button planningArea = new Button( top, SWT.CHECK );
    planningArea.addSelectionListener( new SelectionAdapter(){
      
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
//        int m_selection = SIMPLE;
      }
    });
    
    

    ti.setControl( top );

    return ti;
  }

}
