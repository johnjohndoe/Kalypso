package org.kalypso.ogc.sensor.template;

/**
 * Forwards events to listeners.
 * 
 * @author schlienger
 */
public interface ITemplateAdapter
{
  public void addListener( final ITemplateListener l );
  public void removeListener( final ITemplateListener l );
  
  public void fireTemplateLoaded(  );
}
