package org.kalypso.ogc.sensor.template;


/**
 * @author schlienger
 */
public interface ITemplateEventProvider
{
  public void fireTemplateChanged( TemplateEvent evt );
  
  public void addTemplateEventListener( ITemplateEventListener l );
  public void removeTemplateEventListener( ITemplateEventListener l );
}
