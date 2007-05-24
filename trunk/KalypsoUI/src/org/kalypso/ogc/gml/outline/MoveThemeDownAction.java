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
package org.kalypso.ogc.gml.outline;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.MoveThemeDownCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;

/**
 * @author Stefan Kurzbach
 */
public class MoveThemeDownAction extends MapModellViewActionDelegate
{
  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    final IMapModellView view = getView();
    if( view == null )
      return;

    /* Order the selection as the themes are ordered in the outline. */
    final IKalypsoTheme[] selectedThemesInOrder = getSelectedThemesInOrder( getSelection() );

    /* Move down in reverse order, else it wont happen. */
    for( int i = selectedThemesInOrder.length; i > 0; i-- )
    {
      final IKalypsoTheme kalypsoTheme = selectedThemesInOrder[i - 1];
      moveElementDown( kalypsoTheme, event.display );
    }
  }

  private void moveElementDown( final IKalypsoTheme theme, final Display display )
  {
    final IMapModellView view = getView();
    if( view == null )
      return;

    final IMapModell themeMapModell = theme.getMapModell();

    final MoveThemeDownCommand moveThemeDownCommand = new MoveThemeDownCommand( themeMapModell, theme );
    view.postCommand( moveThemeDownCommand, new SelectThemeRunner( theme, view, display ) );
  }

  public static IKalypsoTheme[] getSelectedThemesInOrder( final ISelection selection )
  {
    final IKalypsoTheme[] selectedThemes = getSelectedThemes( selection );
    /* Wen can only sort within one map model */
    final IMapModell[] selectedModels = getSelectedModels( selectedThemes );
    if( selectedModels.length != 1 )
      return new IKalypsoTheme[0];

    final IMapModell mapModell = selectedModels[0];
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    final List<IKalypsoTheme> allThemesList = new ArrayList<IKalypsoTheme>( Arrays.asList( allThemes ) );

    final List<IKalypsoTheme> selectedThemesList = Arrays.asList( selectedThemes );
    allThemesList.retainAll( selectedThemesList );

    return allThemesList.toArray( new IKalypsoTheme[allThemesList.size()] );
  }

  /**
   * @see org.kalypso.ogc.gml.outline.MapModellViewActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    super.selectionChanged( action, selection );

    final IKalypsoTheme[] selectedThemes = getSelectedThemes( getSelection() );
    final IMapModell[] selectedModels = getSelectedModels( selectedThemes );

    final boolean bEnable;
    if( selectedModels.length != 1 )
      bEnable = false;
    else if( selectedThemes.length > 0 )
    {
      final IMapModell mapModell = selectedModels[0];
      final Object[] elements = mapModell.getAllThemes();
      bEnable = !(elements.length == 0) && !Arrays.asList( selectedThemes ).contains( elements[elements.length - 1] );
    }
    else
      bEnable = false;

    action.setEnabled( bEnable );
  }

  /** Returns the modells (=parents) of the given themes. Filters all duplicates. */
  public static IMapModell[] getSelectedModels( final IKalypsoTheme[] themes )
  {
    final Set<IMapModell> models = new HashSet<IMapModell>( themes.length );

    for( final IKalypsoTheme kalypsoTheme : themes )
      models.add( kalypsoTheme.getMapModell() );

    return models.toArray( new IMapModell[models.size()] );
  }
}
