package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree.model.geometry.GM_Envelope;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public interface IKalypsoTheme extends ModellEventProvider, ModellEventListener
{
  public void dispose();
  
  public void paintSelected( final Graphics g, final GeoTransform p, final double scale,
      final GM_Envelope bbox, final int selectionId );
  
  /**
   * returns the name of the layer
   */
  public String getName();

  public void setName( final String name );
  
  public GM_Envelope getBoundingBox();
}