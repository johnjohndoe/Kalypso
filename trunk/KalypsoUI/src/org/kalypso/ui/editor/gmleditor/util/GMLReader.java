/*
 * Created on Jan 19, 2005
 *  
 */
package org.kalypso.ui.editor.gmleditor.util;

import java.net.URL;
import java.util.List;

import javax.swing.event.EventListenerList;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * @author F.Lindemann
 *  
 */
public class GMLReader implements IPoolListener
{
  private CommandableWorkspace workspace = null;

  private PoolableObjectType m_layerKey = null;

  private EventListenerList listenerList = new EventListenerList();

  private String type = null;

  private String source = null;

  private URL context = null;

  public GMLReader( String m_type, String m_source, URL m_context )
  {
    type = m_type;
    source = m_source;
    context = m_context;
  }

  public void load()
  {
    try
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      m_layerKey = new PoolableObjectType( type, source, context );
      pool.addPoolListener( this, m_layerKey );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public CommandableWorkspace getGMLWorkspace()
  {
    return workspace;
  }

  public FeatureElement getGMLDocument( CommandableWorkspace m_workspace )
  {
    if( m_workspace == null )
      return null;
    Feature feature = m_workspace.getRootFeature();
    FeatureElement element = new FeatureElement( feature );
    recursiveRead( feature, element );

    // adds a root element that holds the entire tree -> for visualization
    // purposes
    return FeatureElement.createRootFeatureElement( element );
  }

  // reads the GMLDocument recursivly into a tree with root "element"
  private void recursiveRead( Feature ft, FeatureElement element )
  {
    FeatureTypeProperty[] ftp = ft.getFeatureType().getProperties();
    if( ftp != null )
    {
      for( int i = 0; i < ftp.length; i++ )
      {
        if( ftp[i] instanceof FeatureAssociationTypeProperty )
        {
          PropertyElement propertyElement = new PropertyElement(
              (FeatureAssociationTypeProperty)ftp[i] );
          element.addProperty( propertyElement );
          try
          {
            if( ft.getProperties()[i] instanceof Feature )
            {
              Feature feature = (Feature)ft.getProperties()[i];
              FeatureElement fe = new FeatureElement( feature );
              propertyElement.addFeature( fe );
              recursiveRead( feature, fe );
            }
            else if( ft.getProperties()[i] instanceof List )
            {
              List ss = (List)ft.getProperties()[i];
              for( int j = 0; j < ss.size(); j++ )
              {
                if( ss.get( j ) instanceof Feature )
                {
                  Feature feature = (Feature)ss.get( j );
                  FeatureElement fe = new FeatureElement( feature );
                  propertyElement.addFeature( fe );
                  recursiveRead( feature, fe );
                }
                else if(ss.get(j) instanceof String)
                {
                  LinkedFeatureElement lfe = new LinkedFeatureElement( ss.get(j).toString() );
                  propertyElement.addLinkedFeature( lfe );                  
                }                   
              }
            }
            else
            {
              if( ft.getProperties()[i] != null
                  && ft.getProperties()[i].toString().trim().length() > 0 )
              {
                LinkedFeatureElement lfe = new LinkedFeatureElement( ft.getProperties()[i]
                    .toString() );
                propertyElement.addLinkedFeature( lfe );
              }
            }
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
  {
    try
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

      if( pool.equalsKeys( key, m_layerKey ) )
      {
        workspace = (CommandableWorkspace)newValue;
      }
    }
    catch( final Throwable e )
    {
      // alles abfangen, damit der Pool trotzdem weitermacht
      e.printStackTrace();
    }
    fire();
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    System.out.println( "objectInvalid" );
  }

  public void addGMLDocumentListener( IGMLDocumentListener l )
  {
    listenerList.add( IGMLDocumentListener.class, l );
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == IGMLDocumentListener.class )
      {
        GMLDocumentEvent event = new GMLDocumentEvent( getGMLDocument( getGMLWorkspace() ),getGMLWorkspace() );
        ( (IGMLDocumentListener)listeners[i + 1] ).onChange( event );
      }
    }
  }
}