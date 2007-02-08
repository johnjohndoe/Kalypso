/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.wizards.imports.baseMap;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ui.wizards.imports.roughness.Messages;


/**
 * @author Madanagopal
 *
 */
public class ImportBaseMapWizard extends Wizard
      implements INewWizard
{
   private IStructuredSelection initialSelection;

   private BaseMapMainPage mPage;
  // private SelectStringsWizardPage selectStringsPage;
   
   /**
    * Construct a new instance and initialize the dialog settings
    * for this instance.
    */
   public ImportBaseMapWizard() {
//      IDialogSettings favoritesSettings =
//         FavoritesPlugin.getDefault().getDialogSettings();
//      IDialogSettings wizardSettings =
//         favoritesSettings.getSection("ExtractStringsWizard");
//      if (wizardSettings == null)
//         wizardSettings =
//            favoritesSettings.addNewSection("ExtractStringsWizard");
//      setDialogSettings(favoritesSettings);
   }
   
   /**
    * Initializes this creation wizard using the passed workbench and
    * object selection. This method is called after the no argument
    * constructor and before other methods are called.
    * 
    * @param workbench the current workbench
    * @param selection the current object selection
    */
   public void init(IWorkbench workbench, IStructuredSelection selection)
   {
      initialSelection = selection;
   }

   public void addPages() {
      setWindowTitle(Messages.getString( "BaseMapWizard.0" ));
      mPage = new BaseMapMainPage();
      addPage(mPage);
//      selectStringsPage = new SelectStringsWizardPage();
//      addPage(selectStringsPage);
      mPage.init(initialSelection);
   }

   /**
    * This method is called by the wizard framework when the user
    * presses the Finish button.
    */
   public boolean performFinish() {

      // Perform the operation in a separate thread
      // so that the operation can be canceled.
      try {
         getContainer().run(true, true, new IRunnableWithProgress() {
            public void run(IProgressMonitor monitor)
               throws InvocationTargetException, InterruptedException
            {
//               performOperation(monitor);
            }

            
         });
      }
      catch (InvocationTargetException e) {
       //  FavoritesLog.logError(e);
         return false;
      }
      catch (InterruptedException e) {
         // User canceled, so stop but donít close wizard.
         return false;
      }
      return true;
   }

   /**
    * Answer the selected source location
    */
   public IPath getSourceLocation() {
      return mPage.getSourceLocation();
   }

}
