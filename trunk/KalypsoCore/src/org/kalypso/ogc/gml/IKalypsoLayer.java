package org.kalypso.ogc.gml;

import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public interface IKalypsoLayer extends ModellEventProvider, ModellEventListener 
{
  public GM_Envelope getBoundingBox();  
  public CS_CoordinateSystem getCoordinatesSystem();
  public void setCoordinatesSystem( CS_CoordinateSystem crs );  
  public String getName();  
  public void addModellListener( final ModellEventListener listener ); 
  public void fireModellEvent( final ModellEvent event );
  public void removeModellListener( ModellEventListener listener );
  public void onModellChange( final ModellEvent modellEvent );
}