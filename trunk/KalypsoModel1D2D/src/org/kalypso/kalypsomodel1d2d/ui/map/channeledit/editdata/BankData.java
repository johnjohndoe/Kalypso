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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import com.vividsolutions.jts.geom.LineString;

/**
 * Represents a bank as part of a segment.
 *
 * @author Gernot Belger
 */
class BankData implements IBankData
{
  /* Original bank line from the user selection, nver changes */
  private final LineString m_originalGeometry;

  /* Original bank geometry, cropped by the intersection points with upstream and downstream profile. */
  private final LineString m_croppedOriginalGeometry;

  /* Segmented bank part with numberOfBankIntersections number of parts. Edited by the user */
  private final LineString m_workingGeometry;

  private final boolean m_isUserChanged;

  private final ISegmentData m_segment;

  public BankData( final ISegmentData segment, final LineString bankLine, final LineString croppedBankLine, final LineString workingGeometry, final boolean isUserChanged )
  {
    m_originalGeometry = bankLine;
    m_croppedOriginalGeometry = croppedBankLine;
    m_workingGeometry = workingGeometry;
    m_isUserChanged = isUserChanged;
    m_segment = segment;
  }

  @Override
  public LineString getWorkingGeometry( )
  {
    return m_workingGeometry;
  }

  @Override
  public boolean isUserChanged( )
  {
    return m_isUserChanged;
  }

  @Override
  public LineString getCroppedOriginalGeometry( )
  {
    return m_croppedOriginalGeometry;
  }

  public LineString getOriginalGeometry( )
  {
    return m_originalGeometry;
  }

  @Override
  public ISegmentData getSegment( )
  {
    return m_segment;
  }
}