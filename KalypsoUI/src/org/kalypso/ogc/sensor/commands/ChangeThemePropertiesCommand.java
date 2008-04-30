/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.commands;

import java.awt.Color;
import java.awt.Stroke;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;

/**
 * Changes the properties of a diagram curve.
 * 
 * @author Gernot Belger
 */
public class ChangeThemePropertiesCommand implements ICommand
{
  private final DiagView m_view;

  private final DiagViewCurve m_curve;

  private final String m_name;

  private final Color m_color;

  private final Stroke m_stroke;

  private String m_oldName;

  private Color m_oldColor;

  private Stroke m_oldStroke;

  public ChangeThemePropertiesCommand( DiagView view, DiagViewCurve curve, final String name, final Color color,
      final Stroke stroke )
  {
    m_view = view;
    m_curve = curve;
    m_name = name;
    m_color = color;
    m_stroke = stroke;

    m_oldName = m_curve.getName();
    m_oldColor = m_curve.getColor();
    m_oldStroke = m_curve.getStroke();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    m_curve.setName( m_name );
    m_curve.setColor( m_color );
    m_curve.setStroke( m_stroke );

    m_view.refreshItemData( m_curve, this );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_curve.setName( m_oldName );
    m_curve.setColor( m_oldColor );
    m_curve.setStroke( m_oldStroke );

    m_view.refreshItemData( m_curve, this );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Kurveneigenschaften verändern";
  }
}