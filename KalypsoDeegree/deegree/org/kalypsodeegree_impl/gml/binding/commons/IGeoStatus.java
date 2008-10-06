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
package org.kalypsodeegree_impl.gml.binding.commons;

import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 */
public interface IGeoStatus extends IStatus, IFeatureWrapperCollection<IGeoStatus>
{
  public static final QName QNAME = new QName( NS.COMMON, "Status" );

  public static final QName QNAME_PROP_STATUS_TIME = new QName( NS.COMMON, "time" );

  public static final QName QNAME_PROP_STATUS_SEVERITY = new QName( NS.COMMON, "severity" );

  public static final QName QNAME_PROP_STATUS_CODE = new QName( NS.COMMON, "code" );

  public static final QName QNAME_PROP_STATUS_PLUGIN = new QName( NS.COMMON, "plugin" );

  public static final QName QNAME_PROP_STATUS_EXCEPTION = new QName( NS.COMMON, "exception" );

  public static final QName QNAME_PROP_STATUS_CHILD_MEMBER = new QName( NS.COMMON, "childMember" );

  public Date getTime( );

  public void setTime( final Date time );

  public void setSeverity( int severity );

  public void setMessage( String message );

  public void setCode( int code );

  public void setException( Throwable t );

  public void setPlugin( String pluginId );
}
