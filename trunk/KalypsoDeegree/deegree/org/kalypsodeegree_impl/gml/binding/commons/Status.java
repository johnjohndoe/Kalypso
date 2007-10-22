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

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 */
public class Status extends AbstractFeatureBinder implements IStatus
{

  public Status( Feature featureToBind )
  {
    super( featureToBind, new QName( NS.COMMON, "Status" ) );
  }

  private final IFeatureWrapperCollection<IStatus> m_children = new FeatureWrapperCollection<IStatus>( getWrappedFeature(), IStatus.class, QNAME_PROP_STATUS_CHILD_MEMBER );

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IStatus#getChildrenCollection()
   */
  public IFeatureWrapperCollection<IStatus> getChildrenCollection( )
  {
    return m_children;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getChildren()
   */
  public org.eclipse.core.runtime.IStatus[] getChildren( )
  {
    return (IStatus[]) m_children.toArray();
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getCode()
   */
  public int getCode( )
  {
    return (Integer) getWrappedFeature().getProperty( QNAME_PROP_STATUS_CODE );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getException()
   */
  public Throwable getException( )
  {
    final String string = (String) getWrappedFeature().getProperty( QNAME_PROP_STATUS_EXCEPTION );
    return new Exception( string );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getMessage()
   */
  public String getMessage( )
  {
    return (String) getWrappedFeature().getProperty( QNAME_PROP_STATUS_EXCEPTION );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getPlugin()
   */
  public String getPlugin( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getSeverity()
   */
  public int getSeverity( )
  {
    final String value = (String) getWrappedFeature().getProperty( QNAME_PROP_STATUS_TYPE );

    if( value == "OK" )
      return 0;

    else if( value == "INFO" )
      return 0x01;

    else if( value == "WARNING" )
      return 0x02;

    else if( value == "ERROR" )
      return 0x04;

    else if( value == "CANCEL" )
      return 0x08;

    else
      return 0;

  }

  /**
   * @see org.eclipse.core.runtime.IStatus#isMultiStatus()
   */
  public boolean isMultiStatus( )
  {
    if( m_children.size() > 0 )
      return true;
    else
      return false;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#isOK()
   */
  public boolean isOK( )
  {
    final String value = (String) getWrappedFeature().getProperty( QNAME_PROP_STATUS_TYPE );
    if( value == "OK" )
      return true;
    else
      return false;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#matches(int)
   */
  public boolean matches( int severityMask )
  {
    if( severityMask == getSeverity() )
      return true;
    else
      return false;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IStatus#getSeverityType()
   */
  public SEVERITYTYPE getSeverityType( )
  {
    final String value = (String) getWrappedFeature().getProperty( QNAME_PROP_STATUS_TYPE );
    return SEVERITYTYPE.valueOf( value );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IStatus#setSeverityType(org.kalypsodeegree_impl.gml.binding.commons.IStatus.SEVERITYTYPE)
   */
  public void setSeverityType( SEVERITYTYPE severityType )
  {
    getWrappedFeature().setProperty( QNAME_PROP_STATUS_TYPE, severityType.name() );

  }

}
