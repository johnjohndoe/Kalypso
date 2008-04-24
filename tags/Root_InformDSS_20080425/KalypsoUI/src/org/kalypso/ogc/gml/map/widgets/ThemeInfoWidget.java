/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.map.widgets;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;

/**
 * @author Gernot Belger
 */
public class ThemeInfoWidget extends AbstractThemeInfoWidget
{
  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( event.getSelection() );
    }
  };

  private ISelectionProvider m_selectionProvider = null;

  public ThemeInfoWidget( )
  {
    super( "Quick-Info", "Zeigt ortsbezogene Informationen zu den selektierten Themen an" );

    setNoThemesTooltip( "<selektieren Sie Themen in der Gliederung>" );
  }

  public ThemeInfoWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    final IWorkbenchPage page = window.getActivePage();
    final GisMapOutlineView outlineView = (GisMapOutlineView) page.findView( GisMapOutlineView.ID );
    if( outlineView == null )
    {
      getMapPanel().setMessage( "Info-Widget : Keine Gliederungsansicht verf�gbar" );
      return;
    }

    final MapPanel outlineMapPanel = outlineView.getMapPanel();
    if( outlineMapPanel != mapPanel )
    {
      getMapPanel().setMessage( "Info-Widget : keine Gliederungsansicht f�r aktive Karte verf�gbar" );
      return;
    }

    m_selectionProvider = outlineView.getSite().getSelectionProvider();
    m_selectionProvider.addSelectionChangedListener( m_selectionListener );

    handleSelectionChanged( m_selectionProvider.getSelection() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    super.finish();

    if( m_selectionProvider != null )
    {
      m_selectionProvider.removeSelectionChangedListener( m_selectionListener );
      m_selectionProvider = null;
    }
  }

  protected void handleSelectionChanged( final ISelection selection )
  {
    final List<IKalypsoTheme> m_themes = new ArrayList<IKalypsoTheme>();

    final IStructuredSelection sel = (IStructuredSelection) selection;
    final Object[] selectedElements = sel.toArray();
    for( final Object object : selectedElements )
    {
      if( object instanceof IKalypsoTheme )
        m_themes.add( (IKalypsoTheme) object );
    }

    setThemes( m_themes.toArray( new IKalypsoTheme[m_themes.size()] ) );

    if( getMapPanel() != null )
      moved( getCurrentPoint() );
  }

}
