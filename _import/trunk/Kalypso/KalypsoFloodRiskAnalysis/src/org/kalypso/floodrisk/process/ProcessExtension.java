/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.process;

import org.eclipse.core.runtime.IPath;
import org.kalypso.services.calculation.job.ICalcJob;

/**
 * ProcessExtension
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class ProcessExtension
{
  private final String m_name;

  private final String m_type;

  private final ICalcJob m_calcJob;

  private final String m_id;

  private final String m_icon;

  private boolean m_calculate = false;

  private IPath m_modelDataPath;

  public ProcessExtension( String name, String type, ICalcJob calcJob, String id, String icon )
  {
    m_name = name;
    m_type = type;
    m_calcJob = calcJob;
    m_id = id;
    m_icon = icon;
  }

  public ICalcJob getCalcJob()
  {
    return m_calcJob;
  }

  //TODO: return icon
  public String getIcon()
  {
    return m_icon;
  }

  public String getId()
  {
    return m_id;
  }

  public String getName()
  {
    return m_name;
  }

  public String getType()
  {
    return m_type;
  }

  public boolean getState()
  {
    return m_calculate;
  }

  public void setState( boolean calculate )
  {
    m_calculate = calculate;
  }

  public IPath getModelDataPath()
  {
    return m_modelDataPath;
  }

  public void setModelDataPath( IPath modelDataPath )
  {
    m_modelDataPath = modelDataPath;
  }
}