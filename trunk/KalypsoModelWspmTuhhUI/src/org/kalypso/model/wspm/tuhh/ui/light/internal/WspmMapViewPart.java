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
package org.kalypso.model.wspm.tuhh.ui.light.internal;

import org.eclipse.ui.ISaveablePart;
import org.eclipse.ui.IViewSite;
import org.kalypso.ui.views.map.MapView;

/**
 * @author Gernot Belger
 */
public class WspmMapViewPart extends MapView implements ISaveablePart
{
  @SuppressWarnings("hiding")
  public static final String ID = "org.kalypso.model.wspm.tuhh.ui.light.internal.WspmMapViewPart"; //$NON-NLS-1$

  //  public static final String PROPERTY_THEME_REACH = "pdbReach"; //$NON-NLS-1$

  @Override
  public void init( final IViewSite site )
  {
    super.init( site );

// PdbWspmUtils.ensureProject();
// final IFile mapFile = PdbWspmUtils.ensureMapFile();
//
// final FileEditorInput input = new FileEditorInput( mapFile );
// setInput( input );
  }

// /* Make sure, that all reaches of the project have a theme in the current map */
// public void updateMap( final TuhhWspmProject project )
// {
// final GisTemplateMapModell mapModell = getMapModell();
// final FindReachThemesVisitor visitor = new FindReachThemesVisitor();
// mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
//
// final CompositeCommand compositeCommand = new CompositeCommand( "Add reach themes" );
//
// final WspmWaterBody[] waterBodies = project.getWaterBodies();
// for( final WspmWaterBody waterBody : waterBodies )
// {
// final WspmReach[] reaches = waterBody.getReaches();
// for( final WspmReach reach : reaches )
// {
// final String reachGmlID = reach.getId();
// if( !visitor.hasReachTheme( reachGmlID ) )
// {
// final AddThemeCommand newTheme = addReachTheme( mapModell, reach );
// if( newTheme != null )
// compositeCommand.addCommand( newTheme );
// }
// }
// }
//
// postCommand( compositeCommand, null );
// }
//
// private AddThemeCommand addReachTheme( final GisTemplateMapModell mapModell, final WspmReach reach )
// {
// final String name = reach.getName();
//    final String type = "gml"; //$NON-NLS-1$
//
//    final String featurePath = String.format( "#fid#%s/%s", reach.getId(), TuhhReach.QNAME_PROP_REACHSEGMENTMEMBER.getLocalPart() ); //$NON-NLS-1$
//
// final String source = IWspmTuhhConstants.FILE_MODELL_GML;
// final AddThemeCommand command = new AddThemeCommand( mapModell, name, type, featurePath, source );
// command.addProperty( PROPERTY_THEME_REACH, reach.getId() );
// command.addProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.FALSE.toString() );
//
// return command;
// }
}