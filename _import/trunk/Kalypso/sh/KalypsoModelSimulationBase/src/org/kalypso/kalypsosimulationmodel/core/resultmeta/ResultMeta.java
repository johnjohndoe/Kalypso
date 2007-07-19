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
package org.kalypso.kalypsosimulationmodel.core.resultmeta;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Thomas Jung
 * 
 */
public class ResultMeta extends AbstractFeatureBinder implements IResultMeta
{

  public ResultMeta( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    // TODO Auto-generated constructor stub
  }

  private static final QName QNAME_PROP_PATH = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_RESULT_NS, "path" );

  private static final QName QNAME_PROP_STATUS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_RESULT_NS, "statusMember" );

  private static final QName QNAME_PROP_CHILDREN = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_RESULT_NS, "statusMember" );

  private static final QName QNAME_PROP_PARENT = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_RESULT_NS, "statusMember" );

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getChildren()
   */
  public IResultMeta[] getChildren( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getParent()
   */
  public IResultMeta getParent( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getPath()
   */
  public String getPath( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getStatus()
   */
  public IStatus getStatus( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setChildren(org.kalypso.kalypsosimulationmodel.core.result.IResultMeta[])
   */
  public void setChildren( IResultMeta[] resultMeta )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setParent(org.kalypso.kalypsosimulationmodel.core.result.IResultMeta)
   */
  public void setParent( IResultMeta resultMeta )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setPath(java.lang.String)
   */
  public void setPath( String path )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setStatus(org.eclipse.core.runtime.IStatus)
   */
  public void setStatus( IStatus status )
  {
    // TODO Auto-generated method stub

  }

}
