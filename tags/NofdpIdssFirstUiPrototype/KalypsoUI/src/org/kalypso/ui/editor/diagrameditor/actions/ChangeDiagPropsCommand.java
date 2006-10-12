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
package org.kalypso.ui.editor.diagrameditor.actions;

import java.util.TimeZone;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.sensor.diagview.DiagView;

/**
 * @author schlienger
 */
public class ChangeDiagPropsCommand implements ICommand
{
  private final String m_diagramTitle;

  private final boolean m_showLegend;

  private final String m_legendTitle;

  private final String m_timezoneName;

  private final DiagView m_diag;

  private String m_orgDiagramTitle;

  private boolean m_orgShowLegend;

  private String m_orgLegendTitle;

  private String m_orgTimezoneName;

  public ChangeDiagPropsCommand( DiagView diag, String diagramTitle, boolean showLegend, String legendTitle, String timezoneName )
  {
    m_orgDiagramTitle = diag.getTitle();
    m_orgShowLegend = diag.isShowLegend();
    m_orgLegendTitle = diag.getLegendName();
    m_orgTimezoneName = diag.getTimezone().getID();

    m_diag = diag;

    m_diagramTitle = diagramTitle;
    m_showLegend = showLegend;
    m_legendTitle = legendTitle;
    m_timezoneName = timezoneName;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    m_diag.setTitle( m_diagramTitle );
    m_diag.setShowLegend( m_showLegend );
    m_diag.setLegendName( m_legendTitle );
    m_diag.setTimezone( TimeZone.getTimeZone( m_timezoneName ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    m_diag.setTitle( m_orgDiagramTitle );
    m_diag.setShowLegend( m_orgShowLegend );
    m_diag.setLegendName( m_orgLegendTitle );
    m_diag.setTimezone( TimeZone.getTimeZone( m_orgTimezoneName ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Diagramm-Eigenschaften ‰ndern";
  }
}
