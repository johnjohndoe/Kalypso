package org.kalypso.afgui.viz;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.editor.mapeditor.GisMapOutlinePage;
import org.kalypso.ui.view.action.KalypsoAddLayerWizard;

public class Utils
{
	GisMapEditor ed;
	public static final String ID_GIS_MAP_EDITOR=
		"org.kalypso.ui.editor.mapeditor.GisMapEditor";
	private static final Logger logger= 
						Logger.getLogger(Logger.class.getName());
     private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

        static
        {
          if( !log )
            logger.setUseParentHandlers( false );
        }
        
	public void importAll(IProject activeProject)
	{
		IWorkbenchPage page = 
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		if(page!=null)
		{
		   
			try
			{
				if(activeProject==null)
				{
					logger.warning("Active project is null");
					return ;
				}
//				IWorkspace ws=ResourcesPlugin.getWorkspace();
				
//				IPath gmtPath=new Path("/.metadata/agger_karte.gmt");
				IFile gmtFile=
					activeProject.getFile("project:/.metadata/agger_karte.gmt");
				if(!gmtFile.exists())
				{
					logger.warning(
							"DO NOT EXISTS:"+gmtFile+
							" pjt="+activeProject);
					return;
				}
				
				
				IWorkbench ui=PlatformUI.getWorkbench();
				ui.getActiveWorkbenchWindow().getPages();
//				IEditorDescriptor eDesc=
//					ui.getEditorRegistry().findEditor(ID_GIS_MAP_EDITOR);
				
				IEditorPart ep=IDE.openEditor(
						page, 
						gmtFile);
				GisMapOutlinePage gmoPage=
					(GisMapOutlinePage)ep.getAdapter(IContentOutlinePage.class);
				
				KalypsoAddLayerWizard wiz=
					new KalypsoAddLayerWizard(gmoPage.getModellView());
				wiz.init( PlatformUI.getWorkbench() );
				//dlg
				WizardDialog wd= 
					new WizardDialog(
							page.getActivePart().getSite().getShell(),
							wiz);
				wd.setTitle("Neue Simulationsmodel");
//				int decision=wd.open();
			}
			catch (PartInitException e)
			{
				logger.log(Level.SEVERE,"/test/Karte.gmt",e);
			}
		}
	}
}
