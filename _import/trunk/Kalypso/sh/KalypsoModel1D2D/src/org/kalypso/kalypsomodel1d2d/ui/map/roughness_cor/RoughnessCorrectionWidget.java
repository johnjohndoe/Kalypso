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
package org.kalypso.kalypsomodel1d2d.ui.map.roughness_cor;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

/**
 * Widget which provides mechanism for modelling 
 * roughness correction
 * 
 * @author Patrice Congo
 */
public class RoughnessCorrectionWidget implements IWidgetWithOptions, IWidget
{

  private RoughnessCorrectionDataModel dataModel = 
                  new RoughnessCorrectionDataModel();

  private RoughnessCorrectionWidgetFace widgetFace = 
                  new RoughnessCorrectionWidgetFace( dataModel );

  private final String name;

  private final String tooltip;

  /**
   * Current strategy for the widget functionality
   */
  private IWidget strategy;
  
  public RoughnessCorrectionWidget( )
  {
    this.name = "Rauheit korrigieren";
    this.tooltip = "Rauheit korrigieren";
  }

  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    try
    {
      return widgetFace.createControl( parent );
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( widgetFace != null )
    {
      widgetFace.disposeControl();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    IMapModell mapModell = mapPanel.getMapModell();
    dataModel.setMapModell( mapModell );
    dataModel.setMapPanel( mapPanel );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    if( strategy != null )
    {
      strategy.clickPopup( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedLeft( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedRight( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    if( strategy != null )
    {
      strategy.dragged( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    if( strategy != null )
    {
      strategy.finish();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return name;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return tooltip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyPressed( e );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyReleased( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyTyped( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.leftClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    if( strategy != null )
    {
      strategy.leftPressed( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.leftReleased( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.middleClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    if( strategy != null )
    {
      strategy.middlePressed( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.middleReleased( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    if( strategy != null )
    {
      strategy.moved( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    if( strategy != null )
    {
      strategy.paint( g );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    if( strategy != null )
    {
      strategy.rightClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    if( strategy != null )
    {
      strategy.rightPressed( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    if( strategy != null )
    {
      strategy.rightReleased( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( strategy != null )
    {
      strategy.setSelection( selection );
    }
  }

}
