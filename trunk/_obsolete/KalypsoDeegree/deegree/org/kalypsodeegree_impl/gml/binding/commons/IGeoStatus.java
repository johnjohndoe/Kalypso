/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
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
