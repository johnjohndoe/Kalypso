package org.kalypso.ogc.sensor.template;

import java.util.EventListener;

/**
 * @author schlienger
 */
public interface ITemplateEventListener extends EventListener
{
  public void onTemplateChanged( TemplateEvent evt );
}