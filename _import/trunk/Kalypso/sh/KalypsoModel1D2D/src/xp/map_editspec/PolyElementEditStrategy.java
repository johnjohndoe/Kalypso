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
package xp.map_editspec;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;

import xp.IMapEditStrategy;

/**
 * Privide the editing strategy for creating deleting and
 * changing elements
 * 
 * @author Patrice Congo
 */
public class PolyElementEditStrategy 
                  implements IMapEditStrategy
{
  
  private String name;
  private String toolTip;
  private IResource iconResource;
  
  /**
   * internal root panel for strategy gui representation
   */
  private Composite panel;
  
  
  /**
   * @see xp.IMapEditSpecification#createControl(java.awt.Composite)
   */
  public void createControl( Composite parent )
  {
   panel= new Composite(parent, SWT.FILL);
   Label l= new Label(parent, SWT.FILL);
   l.setText( "Element draw" );
  }

  /**
   * @see xp.IMapEditSpecification#hasVisualRepresentation()
   */
  public boolean hasVisualRepresentation( )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return null;
  }
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    
  }

  /**
   * @see xp.IMapEditStrategy#getIconResource()
   */
  public IResource getIconResource( )
  {
    return iconResource;
  }

  /**
   * @see xp.IMapEditStrategy#setIconResource(org.eclipse.core.resources.IResource)
   */
  public void setIconResource( IResource iconResource )
  {
    this.iconResource=iconResource;
  }

  /**
   * @see xp.IMapEditStrategy#setName(java.lang.String)
   */
  public void setName( String name ) throws IllegalArgumentException
  {
    this.name=name; 
  }

  /**
   * @see xp.IMapEditStrategy#setToolTip(java.lang.String)
   */
  public void setToolTip( String tooltip )
  {
    this.toolTip=tooltip;
  }
  
  
}
