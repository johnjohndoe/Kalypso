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
package org.kalypso.services.ocs.repository;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.services.proxy.DataBean;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.zml.ObservationType;
import org.xml.sax.InputSource;

/**
 * An IObservation that comes from the Kalypso OCS.
 * 
 * @author schlienger
 */
public class ServiceRepositoryObservation implements IObservation
{
  private final IObservationService m_srv;

  private final ObservationBean m_ob;

  private IObservation m_obs = null;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter(
      this );

  public ServiceRepositoryObservation( final IObservationService srv,
      final ObservationBean ob )
  {
    m_srv = srv;
    m_ob = ob;
  }

  /**
   * Lazy loading.
   * 
   * @param args
   * @return IObservation loaded from the server
   * 
   * @throws SensorException
   */
  private IObservation getRemote( final IVariableArguments args )
      throws SensorException
  {
    if( args == null && m_obs != null )
      return m_obs;

    m_obs = loadFromServer( args );

    return m_obs;
  }

  /**
   * Uses the webservice to request the observation.
   * 
   * @return IObservation loaded from the server
   */
  private IObservation loadFromServer( final IVariableArguments args )
      throws SensorException
  {
    String href = m_ob.getId();
    if( args instanceof DateRangeArgument )
      href = ZmlURL.insertDateRange( href, (DateRangeArgument)args );

    InputStream ins = null;

    try
    {
      final DataBean db = m_srv.readData( href );

      ins = db.getDataHandler().getInputStream();
      final IObservation obs = ZmlFactory.parseXML( new InputSource( ins ), "",
          null );
      ins.close();

      m_srv.clearTempData( db.getId() );

      return obs;
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  public String getIdentifier()
  {
    return ZmlURL.addServerSideId( m_ob.getId() );
  }

  public String getName()
  {
    return m_ob.getName();
  }

  public boolean isEditable()
  {
    try
    {
      return getRemote( null ).isEditable();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return false;
    }
  }

  public IXlink getTarget()
  {
    try
    {
      return getRemote( null ).getTarget();
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    if( m_obs != null )
      return m_obs.getMetadataList();

    final MetadataList md = new MetadataList();
    md.putAll( m_ob.getMetadataList() );
    return md;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    try
    {
      return getRemote( null ).getAxisList();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public synchronized ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    ITuppleModel values = ObservationCache.getInstance().getValues( this );

    if( values == null )
    {
      values = getRemote( args ).getValues( null );

      ObservationCache.getInstance().addValues( this, values );
    }

    return values;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    // sets values
    final IObservation obs = getRemote( new DateRangeArgument() );
    obs.setValues( values );

    Writer fw = null;

    try
    {
      // save zml
      final ObservationType obst = ZmlFactory.createXML( obs, null );

      final File tmpFile = File.createTempFile( "towards-server", "zml" );
      tmpFile.deleteOnExit();

      final FileOutputStream stream = new FileOutputStream( tmpFile );
      final Marshaller marshaller = ZmlFactory.getMarshaller();
      final String enc = marshaller.getProperty( Marshaller.JAXB_ENCODING )
          .toString();
      fw = new OutputStreamWriter( stream, enc );

      marshaller.marshal( obst, fw );
      fw.close();

      // let server read file and save on its own
      m_srv.writeData( m_ob, new DataHandler( new FileDataSource( tmpFile ) ) );

      // and clean temp stuff
      tmpFile.delete();

      m_evtPrv.fireChangedEvent();
    }
    catch( Exception e ) // generic for simplicity
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( fw );
    }
  }

  /**
   * Sets the given values to the server side observation defined by the given
   * href.
   */
  public static void setValuesFor( final ITuppleModel values,
      final String href, final IObservationService srv ) throws SensorException
  {
    final ServiceRepositoryObservation srvObs = new ServiceRepositoryObservation(
        srv, new ObservationBean( href, "", null ) );

    srvObs.setValues( values );
  }

  /**
   * Reads the file as a ZML-File and sets the values of the parsed observation
   * to the server one defined by the given href.
   */
  public static void setValuesFor( final IFile file, final String href,
      final IObservationService srv ) throws SensorException
  {
    InputStreamReader in = null;
    try
    {
      in = new InputStreamReader( file.getContents(), file.getCharset() );
      final IObservation obs = ZmlFactory.parseXML( new InputSource( in ), "",
          null );
      in.close();

      setValuesFor( obs.getValues( null ), href, srv );
    }
    catch( final Exception e )
    {
      if( e instanceof SensorException )
        throw (SensorException)e;

      e.printStackTrace();
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( in );
    }
  }

  public void addListener( IObservationListener listener )
  {
    m_evtPrv.addListener( listener );
  }

  public void removeListener( IObservationListener listener )
  {
    m_evtPrv.removeListener( listener );
  }

  public void clearListeners()
  {
    m_evtPrv.clearListeners();
  }

  public String getHref()
  {
    return getIdentifier();
  }
}