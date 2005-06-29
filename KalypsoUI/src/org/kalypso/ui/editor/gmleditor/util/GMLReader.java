package org.kalypso.ui.editor.gmleditor.util;

import java.net.URL;
import java.util.Iterator;
import java.util.List;

import javax.swing.event.EventListenerList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.loader.IPooledObject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * @author F.Lindemann
 *  
 */
public class GMLReader implements IPoolListener, ICommandTarget, IPooledObject
{
  private CommandableWorkspace m_workspace = null;

  private final PoolableObjectType m_layerKey;

  private final EventListenerList listenerList = new EventListenerList();

  private JobExclusiveCommandTarget m_commandTarget;

  private boolean m_loaded = false;

  public GMLReader( final String type, final String source, final URL context )
  {
    m_layerKey = new PoolableObjectType( type, source, context );

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    try
    {
      pool.addPoolListener( this, m_layerKey );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      pool.removePoolListener( this );
    }
  }

  public CommandableWorkspace getGMLWorkspace()
  {
    return m_workspace;
  }

  public static FeatureElement getGMLDocument( final CommandableWorkspace workspace )
  {
    if( workspace == null )
      return null;

    final FeatureElement rootElement = FeatureElement.createRootFeature();
    final Feature feature = workspace.getRootFeature();
    final FeatureElement element = new FeatureElement( rootElement, feature );
    createFeatureChildren( element, feature );
    return rootElement;
  }

  public static void createFeatureChildren( final FeatureElement parent, final Feature ft )
  {
    // children erzeugen
    final FeatureTypeProperty[] ftp = ft.getFeatureType().getProperties();
    if( ftp != null )
    {
      for( int i = 0; i < ftp.length; i++ )
      {
        if( ftp[i] instanceof FeatureAssociationTypeProperty )
        {
          final PropertyElement propertyElement = new PropertyElement( parent, (FeatureAssociationTypeProperty)ftp[i] );
          try
          {
            final Object value = ft.getProperties()[i];
            if( value instanceof Feature )
            {
              final Feature feature = (Feature)value;
              final FeatureElement fe = new FeatureElement( propertyElement, feature );
              createFeatureChildren( fe, feature );
            }
            else if( value instanceof List )
            {
              final List ss = (List)value;
              for( final Iterator listIt = ss.iterator(); listIt.hasNext(); )
              {
                final Object next = listIt.next();
                if( next instanceof Feature )
                {
                  final Feature f = (Feature)next;
                  final FeatureElement element = new FeatureElement( propertyElement, f );
                  createFeatureChildren( element, f );
                }
                else if( next instanceof String )
                  new LinkedFeatureElement( propertyElement, next.toString() );
              }
            }
            else
            {
              if( value != null && value.toString().trim().length() > 0 )
                new LinkedFeatureElement( propertyElement, value.toString() );
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
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
  {
    try
    {
      if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
      {
        m_workspace = (CommandableWorkspace)newValue;
        m_commandTarget = new JobExclusiveCommandTarget( m_workspace, null );
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
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    System.out.println( "objectInvalid" );
    m_loaded = false;
  }

  public void removeGMLDocuentListener( final IGMLDocumentListener l )
  {
    listenerList.remove( IGMLDocumentListener.class, l );
  }

  public void addGMLDocumentListener( final IGMLDocumentListener l )
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
        GMLDocumentEvent event = new GMLDocumentEvent( getGMLDocument( getGMLWorkspace() ), getGMLWorkspace() );
        ( (IGMLDocumentListener)listeners[i + 1] ).onChange( event );
      }
    }
  }

  public void dispose()
  {
    KalypsoGisPlugin.getDefault().getPool().removePoolListener( this );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  public void saveFeatures( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      if( m_workspace != null )
        KalypsoGisPlugin.getDefault().getPool().saveObject( m_workspace, monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Speichern", e ) );
    }
  }

  /**
   * @see org.kalypso.loader.IPooledObject#isLoaded()
   */
  public boolean isLoaded()
  {
    if( m_loaded )
      return true;
    // workspace not here
    if( m_workspace == null )
      return false;
    m_loaded = true;
    return m_loaded;
  }
}