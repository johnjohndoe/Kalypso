package org.kalypso.ogc.gml;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.services.wms.RemoteWMService;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;
import org.kalypso.ogc.gml.event.ModellEventProviderAdapter;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class KalypsoWMSLayer implements IKalypsoLayer
{
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private final String myName;
  private final String myLayers;
  private CS_CoordinateSystem myCRS;
  private final RemoteWMService myRemoteWMS; 
 
  public KalypsoWMSLayer( final String name, final String layers, final CS_CoordinateSystem crs,final RemoteWMService remoteWMS)
  {
    myName = name;
    myLayers=layers;
    myCRS=crs;
    myRemoteWMS=remoteWMS;
  }

  public RemoteWMService getRemoteWMService()
  {
    return myRemoteWMS;
  }
  
  public String getLayerList()
  {
    return myLayers;
  }
  public GM_Envelope getBoundingBox()
  {
    return null;// TODO
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCRS;
  }

  public String getName()
  {
    return myName;
  }

  public void setCoordinatesSystem( CS_CoordinateSystem crs )
  {
    myCRS=crs;
  }
  
  public void addModellListener( final ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.event.ModellEventListener#onModellChange(org.kalypso.ogc.gml.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  } 
}