package org.kalypso.ui.editor.gistableeditor.actions;

import java.io.IOException;
import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.ogc.gml.table.wizard.ExportTableBerichtWizard;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.services.proxy.IMetaDocService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author Belger
 */
public class ExportBerichtActionDelegate extends GisTableAbstractActionDelagate
{
  private IMetaDocService m_service;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    try
    {
      final DocBean doc = prepareService();

      final Wizard exportWizard = new ExportTableBerichtWizard( getEditor().getLayerTable(), doc );

      final WizardDialog dialog = new WizardDialog( getEditor().getSite().getShell(), exportWizard );
      if( dialog.open() == Window.OK )
        saveData( doc );
      else
      {
        cancelData( doc );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      ErrorDialog.openError( getEditor().getSite().getShell(), "Fehler",
          "Bericht konnte nicht exportiert werden", e.getStatus() );
    }
  }

  private void cancelData( final DocBean doc ) throws CoreException
  {
    try
    {
      m_service.rollbackNewDocument( doc );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Löschen der Berichtsvorlage auf dem Server gescheitert", e ) );
    }
  }

  private void saveData( final DocBean bean ) throws CoreException
  {
    try
    {
      m_service.commitNewDocument( bean );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Berichtsablage gescheitert", e ) );
    }
  }

  private DocBean prepareService() throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault()
          .getServiceProxyFactory();
      m_service = (IMetaDocService)serviceProxyFactory.getProxy( "Kalypso_MetaDocService",
          ClassUtilities.getOnlyClassName( IMetaDocService.class ) );

      return m_service.prepareNewDocument( ".csv" );
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

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#refreshAction()
   */
  protected void refreshAction()
  {
    // nix tun, immer Aktiv
  }
}