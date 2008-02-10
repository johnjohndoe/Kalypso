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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import com.sun.xml.messaging.saaj.util.ByteInputStream;

/**
 * The feature based implementation of {@link IGeoStatus}.
 * 
 * @author Thomas Jung
 */
public class GeoStatus extends FeatureWrapperCollection<IGeoStatus> implements IGeoStatus
{
  private enum SEVERITYTYPE
  {
    ok,
    info,
    warning,
    error,
    cancel
  }

  private final IFeatureWrapperCollection<IGeoStatus> m_children = new FeatureWrapperCollection<IGeoStatus>( getFeature(), IGeoStatus.class, QNAME_PROP_STATUS_CHILD_MEMBER );

  public GeoStatus( final Feature featureToBind )
  {
    super( featureToBind, IGeoStatus.class, QNAME_PROP_STATUS_CHILD_MEMBER );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IStatus#getChildrenCollection()
   */
  public IFeatureWrapperCollection<IGeoStatus> getChildrenCollection( )
  {
    return m_children;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getChildren()
   */
  public org.eclipse.core.runtime.IStatus[] getChildren( )
  {
    return m_children.toArray( new IGeoStatus[m_children.size()] );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getCode()
   */
  public int getCode( )
  {
    return getProperty( QNAME_PROP_STATUS_CODE, Integer.class );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getException()
   */
  public Throwable getException( )
  {
    // REMARK: we do deserialize the exception from a byte stream which was obtained as string from the gml

    try
    {
      final String encodedString = (String) getFeature().getProperty( QNAME_PROP_STATUS_EXCEPTION );

      final byte[] encodedBytes = encodedString.getBytes( "UTF-8" );

      // REMARK: Although this property is defined as 'string' we decode is as base64
      final byte[] bytes = Base64.decodeBase64( encodedBytes );

      final ByteInputStream bis = new ByteInputStream( bytes, bytes.length );
      final ObjectInputStream ois = new ObjectInputStream( bis );
      final Throwable t = (Throwable) ois.readObject();
      ois.close();

      return t;
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoDeegreePlugin.getDefault().getLog().log( status );
    }

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getMessage()
   */
  public String getMessage( )
  {
    return getDescription();
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getPlugin()
   */
  public String getPlugin( )
  {
    return getProperty( QNAME_PROP_STATUS_PLUGIN, String.class );
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#getSeverity()
   */
  public int getSeverity( )
  {
    final SEVERITYTYPE severityType = getSeverityType();
    switch( severityType )
    {
      case ok:
        return OK;
      case info:
        return INFO;
      case warning:
        return WARNING;
      case error:
        return ERROR;
      case cancel:
        return CANCEL;

      default:
        throw new IllegalStateException();
    }
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#isMultiStatus()
   */
  public boolean isMultiStatus( )
  {
    return m_children.size() > 0;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#isOK()
   */
  public boolean isOK( )
  {
    return getSeverityType() == SEVERITYTYPE.ok;
  }

  /**
   * @see org.eclipse.core.runtime.IStatus#matches(int)
   */
  public boolean matches( final int severityMask )
  {
    return (severityMask & getSeverity()) != 0;
  }

  private SEVERITYTYPE getSeverityType( )
  {
    final String value = (String) getFeature().getProperty( QNAME_PROP_STATUS_SEVERITY );

    return SEVERITYTYPE.valueOf( value );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setCode(int)
   */
  public void setCode( final int code )
  {
    setProperty( QNAME_PROP_STATUS_CODE, code );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setException(java.lang.Throwable)
   */
  public void setException( final Throwable t )
  {
    // REMARK: we do serialize the exception as a byte stream and write it as string into the gml

    ObjectOutputStream oos = null;
    try
    {
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      oos = new ObjectOutputStream( bos );
      oos.writeObject( t );
      oos.close();

      // REMARK: Although this property is defined as 'string' we decode is as base64
      final byte[] bytes = bos.toByteArray();
      final byte[] encodedBytes = Base64.encodeBase64( bytes );

      final String encodedThrowable = new String( encodedBytes, "UTF-8" );
      setProperty( QNAME_PROP_STATUS_EXCEPTION, encodedThrowable );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoDeegreePlugin.getDefault().getLog().log( status );
    }
    finally
    {
      IOUtils.closeQuietly( oos );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setMessage(java.lang.String)
   */
  public void setMessage( final String message )
  {
    setDescription( message );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setPlugin(java.lang.String)
   */
  public void setPlugin( final String pluginId )
  {
    setProperty( QNAME_PROP_STATUS_PLUGIN, pluginId );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setSeverity(int)
   */
  public void setSeverity( final int severity )
  {
    final SEVERITYTYPE severityType = toSeverityType( severity );
    setSeverityType( severityType );
  }

  private SEVERITYTYPE toSeverityType( final int severity )
  {
    switch( severity )
    {
      case OK:
        return SEVERITYTYPE.ok;

      case INFO:
        return SEVERITYTYPE.info;

      case WARNING:
        return SEVERITYTYPE.warning;

      case ERROR:
        return SEVERITYTYPE.error;

      case CANCEL:
        return SEVERITYTYPE.cancel;

      default:
        throw new IllegalArgumentException( "Unknown severity: " + severity );
    }
  }

  private void setSeverityType( final SEVERITYTYPE severityType )
  {
    setProperty( QNAME_PROP_STATUS_SEVERITY, severityType.name() );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#getTime()
   */
  public Date getTime( )
  {
    final XMLGregorianCalendar cal = getProperty( QNAME_PROP_STATUS_TIME, XMLGregorianCalendar.class );
    return DateUtilities.toDate( cal );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus#setTime(java.util.Date)
   */
  public void setTime( final Date time )
  {
    final XMLGregorianCalendar cal = DateUtilities.toXMLGregorianCalendar( time );
    setProperty( QNAME_PROP_STATUS_TIME, cal );
  }
}
