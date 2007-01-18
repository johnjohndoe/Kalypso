package org.kalypso.kalypsosimulationmodel.wizard.shapeImport;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;


public class DataContainer {
	protected String inputFile;
	protected String outputFile;
	protected String shapeProperty;
	protected String coordinateSystem;
	protected String description;
	protected IProject project;
	
	public String getInputFile() {
		return inputFile;
	}
	public URL getInputFileURL() {
		try {
			return new URL("file:"+inputFile); //$NON-NLS-1$
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		return null;
	}
	public String getOutputFile() {
		return outputFile;
	}
	public URL getOutputFileURL() {
		try {
			return new URL("file:"+outputFile); //$NON-NLS-1$
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		return null;
	}
	public String getShapeProperty() {
		return shapeProperty;
	}
	public CS_CoordinateSystem getCoordinateSystem() {
		return ConvenienceCSFactory.getInstance().getOGCCSByName(coordinateSystem);
	}
	public IProject getProject() {
		return project;
	}
	public String getDescription() {
		return description;
	}
}
