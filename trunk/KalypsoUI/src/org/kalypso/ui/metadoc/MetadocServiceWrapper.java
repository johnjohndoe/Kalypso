package org.kalypso.ui.metadoc;

import java.io.IOException;
import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.services.proxy.IMetaDocService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Wrapps the service to provide utility functions
 * 
 * @author belger
 */
public class MetadocServiceWrapper
{
  private final IMetaDocService m_service;
  private final DocBean m_bean;
  
  public MetadocServiceWrapper( final String fileExtension ) throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault()
          .getServiceProxyFactory();
      m_service = (IMetaDocService) serviceProxyFactory.getProxy(
          "Kalypso_MetaDocService", ClassUtilities
              .getOnlyClassName( IMetaDocService.class ) );

      m_bean = m_service.prepareNewDocument( fileExtension );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Aufruf des Rechendienstes", e ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Rechendienst konnte nicht initialisiert werden", e ) );
    }
  }
  
  public void commitData( ) throws CoreException
  {
    try
    {
      m_service.commitNewDocument( m_bean );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Berichtsablage gescheitert", e ) );
    }
  }

  public void cancelData( ) throws CoreException
  {
    try
    {
      m_service.rollbackNewDocument( m_bean );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "L�schen der Berichtsvorlage auf dem Server gescheitert", e ) );
    }
  }

  public DocBean getDoc()
  {
    return m_bean;
  }
}
