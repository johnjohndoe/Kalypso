package org.kalypso.ogc.gml.outline;

import java.io.File;
import java.io.StringReader;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.xml.XMLTools;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.w3c.dom.Document;

/**
 * @author belger
 */
public class SaveStyleAction extends AbstractOutlineAction {
	private Shell shell = null;

	public SaveStyleAction(final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer outlineViewer) 
	{
		super(text, image, tooltipText, outlineViewer, null);
		shell = outlineViewer.getControl().getShell();
	}

	/**
	 * @see org.eclipse.jface.action.Action#run()
	 */

	public void run() 
	{
		Object o = ((IStructuredSelection) getOutlineviewer().getSelection()).getFirstElement();
		if (o instanceof ThemeStyleTreeObject) {
			IKalypsoLayer layer = ((ThemeStyleTreeObject) o).getTheme().getLayer();
			if (layer instanceof KalypsoFeatureLayer) 
			{
				KalypsoUserStyle kalypsoStyle = ((ThemeStyleTreeObject) o).getStyle();
				saveUserStyle(kalypsoStyle,shell);
			}
		}
	}

	public static void saveUserStyle(KalypsoUserStyle userStyle, Shell shell) 
	{
		String[] filterExtension = { "sld" };
		FileDialog saveDialog = new FileDialog(shell, SWT.SAVE);
		saveDialog.setFilterExtensions(filterExtension);
		String sldContents = "<StyledLayerDescriptor version=\"String\" xmlns=\"http://www.opengis.net/sld\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><NamedLayer><Name>deegree style definition</Name>";
		sldContents += ((KalypsoUserStyle) userStyle).exportAsXML();
		sldContents += "</NamedLayer></StyledLayerDescriptor>";
		StyledLayerDescriptor sld;
		try 
		{
			sld = SLDFactory.createSLD(sldContents);
			String filename = saveDialog.open();
			if (filename != null) {
				Document doc = XMLTools.parse(new StringReader(((StyledLayerDescriptor_Impl) sld).exportAsXML()));
				Source source = new DOMSource(doc);
				File file = null;

				if (filename.indexOf(".") == -1)
					file = new File(filename + "." + filterExtension[0]);
				else
					file = new File(filename.substring(0, filename.indexOf("."))+ "." + filterExtension[0]);
				Result result = new StreamResult(file);
				Transformer xformer = TransformerFactory.newInstance().newTransformer();
				xformer.transform(source, result);
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}	
	
	protected final void refresh() 
	{
		boolean bEnable = false;

		final IStructuredSelection s = (IStructuredSelection) getOutlineviewer().getSelection();

		if (s.getFirstElement() instanceof ThemeStyleTreeObject)
			bEnable = true;

		setEnabled(bEnable);
	}
}