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
package org.kalypso.ui.rrm.internal.scenarios;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

import de.renew.workflow.connector.cases.IScenario;

/**
 * @author Holger Albert
 */
public class CompareData
{
  private final IScenario m_scenario;

  private final String m_key;

  private final IFile m_file;

  private final GMLXPath[] m_listPaths;

  private final QName[] m_uniqueProperties;

  public CompareData( final IScenario scenario, final String key, final IFile file, final GMLXPath[] listPaths, final QName[] uniqueProperties )
  {
    m_scenario = scenario;
    m_key = key;
    m_file = file;
    m_listPaths = listPaths;
    m_uniqueProperties = uniqueProperties;
  }

  public IScenario getScenario( )
  {
    return m_scenario;
  }

  public String getKey( )
  {
    return m_key;
  }

  public IFile getFile( )
  {
    return m_file;
  }

  public GMLXPath[] getListPaths( )
  {
    return m_listPaths;
  }

  public QName[] getUniqueProperties( )
  {
    return m_uniqueProperties;
  }
}