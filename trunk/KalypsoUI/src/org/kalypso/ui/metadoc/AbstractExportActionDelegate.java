/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.metadoc;

import java.lang.reflect.Constructor;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractMapEditorActionDelegate;
import org.kalypso.ui.metadoc.util.MetadocServiceWrapper;

/**
 * AbstractExportActionDelegate
 * 
 * @author schlienger
 */
public abstract class AbstractExportActionDelegate extends AbstractMapEditorActionDelegate
{
  private final static Class[] CONS_SIGN = { IExportableDocument.class, DocBean.class };
  private MetadocServiceWrapper m_metadocService;

  /**
   * Performs the export action
   * 
   * @param wizardClass
   * @param expDoc
   * @param shell
   */
  public void runExportAction( final Class wizardClass, final IExportableDocument expDoc, final Shell shell )
  {
    try
    {
      final String username = System.getProperty( "user.name" );
      
      m_metadocService = new MetadocServiceWrapper(  );
      final DocBean doc = m_metadocService.prepareDocument( expDoc.getDocumentExtension(), username );

      final Constructor constructor = wizardClass.getConstructor( CONS_SIGN );
      final Wizard exportWizard = (Wizard) constructor.newInstance( new Object[] {expDoc, doc} ); 

      final WizardDialog dialog = new WizardDialog( shell, exportWizard );
      if( dialog.open() == Window.OK )
        m_metadocService.commitData( doc );
      else
        m_metadocService.cancelData( doc );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      ErrorDialog.openError( shell, "Fehler",
          "Bericht konnte nicht exportiert werden", KalypsoGisPlugin.createErrorStatus("", e ) );
    }
  }
}