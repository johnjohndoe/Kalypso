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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelComposite extends Composite
{
  private final CreateChannelData m_data;

  private final CreateMainChannelWidget m_widget;

  public CreateMainChannelComposite( final Composite parent, final int style, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    super( parent, style );

    m_data = data;
    m_widget = widget;

    try
    {
      init();
    }
    catch( Throwable e )
    {
      e.printStackTrace();
    }
  }

  private void init( )
  {
    /* Retrieve data */
    final IKalypsoFeatureTheme[] profileThemes = m_data.getProfileThemes();

    /* Create gui */
    setLayout( new GridLayout( 2, false ) );

    final ComboViewer viewer = new ComboViewer( this, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );

    final IKalypsoFeatureTheme profileTheme = m_data.getProfileTheme();

    final IKalypsoFeatureTheme themeToSelect;
    if( profileThemes.length == 0 )
    {
      viewer.getControl().setEnabled( false );
      String msg = "<kein Profilthema in Karte vorhanden>";
      viewer.setInput( new String[] { msg } );
      viewer.setSelection( new StructuredSelection( msg ) );
      themeToSelect = null;
    }
    else
    {
      viewer.setInput( profileThemes );

      if( profileTheme != null )
        themeToSelect = profileTheme;
      else
        themeToSelect = profileThemes[0];

      viewer.setSelection( new StructuredSelection( themeToSelect ) );
    }

    if( profileTheme != themeToSelect )
      m_data.setProfileTheme( themeToSelect );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setProfileTheme( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    final Button chooseProfilesButton = new Button( this, SWT.TOGGLE );
    chooseProfilesButton.setText( "Profile ausw‰hlen..." );
    chooseProfilesButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( chooseProfilesButton.getSelection() )
        {
          final IRectangleMapFunction function = new ProfileSelectorFunction( m_data );
          final IWidget selectProfileWidget = new SelectionWidget( "", "", function );
          m_widget.setDelegate( selectProfileWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

  }

}
