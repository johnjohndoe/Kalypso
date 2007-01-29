package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import java.awt.Color;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.eclipse.core.resources.IProject;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class DataContainer {

  public final static String GAUS_KRUEGER = "EPSG:31467";
  
  private String                    m_inputFile;
  private String                    m_outputFile;
  private String                    m_outputDirectory; //relative to project
  private String                    m_shapeProperty;
  private CS_CoordinateSystem       m_coordinateSystem;
  private CS_CoordinateSystem       m_defaultCoordinateSystem;
  private String                    m_description;
  private IProject                  m_project;
  private boolean                   m_createMap = false;
  private HashMap<String, Color>    m_filterPropertyColorMap;

  public DataContainer() {
    super();
    this.m_defaultCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName(GAUS_KRUEGER);
  }
  public final void setInputFile(String inputFile) {
    this.m_inputFile = inputFile;
  }
  public final void setOutputFile(String outputFile) {
    if(outputFile == null || outputFile.length() == 0)
      this.m_outputFile = FileUtilities.nameWithoutExtension(FileUtilities.nameFromPath(this.m_inputFile)) + ".gml";
    if (outputFile.lastIndexOf(".gml") > 0)
      this.m_outputFile = outputFile;
    else
      this.m_outputFile = outputFile + ".gml";
  }
  public final void setOutputDirectory(String directory) {
    m_outputDirectory = directory;
  }
  public final void setProject(IProject project) {
    this.m_project = project;
  }
  public final void setShapeProperty(String shapeProperty) {
    this.m_shapeProperty = shapeProperty;
  }
  public final void setCoordinateSystem(String coordinateSystem) {
    this.m_coordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName(coordinateSystem);
  }
  public final void setCreateMap(boolean createMap) {
    this.m_createMap = createMap;
  }
  public final void setDescription(String description) {
    this.m_description = description;
  }
  public final void setFilterPropertyColorMap(HashMap<String, Color> propertyColorMap) {
    m_filterPropertyColorMap = propertyColorMap;
  }
  public String getInputFile() {
    return m_inputFile;
  }
  public URL getInputFileURL() {
    try {
      return new URL("file:"+m_inputFile); //$NON-NLS-1$
    } catch (MalformedURLException e) {
      e.printStackTrace();
    }
    return null;
  }
  public String getOutputFile() {
    return m_outputFile;
  }
  public URL getOutputFileURL() {
    try {
      return new URL("file:"+m_outputFile); //$NON-NLS-1$
    } catch (MalformedURLException e) {
      e.printStackTrace();
    }
    return null;
  }
  public final String getOutputFileRelativePath() {
    return "project:" + File.separator + m_outputDirectory + FileUtilities.nameFromPath(m_outputFile);
  }
  public String getShapeProperty() {
    return m_shapeProperty;
  }
  public CS_CoordinateSystem getCoordinateSystem(boolean getDefaultIfNull) {
    if(m_coordinateSystem == null && getDefaultIfNull)
      return m_defaultCoordinateSystem;
    else
      return m_coordinateSystem;
  }
  public IProject getProject() {
    return m_project;
  }
  public String getDescription() {
    return m_description;
  }
  public boolean doCreateMap() {
    return m_createMap;
  }
  public URL getMapFileURL() {
    try {
      return new URL("file:"+FileUtilities.nameWithoutExtension(m_outputFile) + ".gmt"); //$NON-NLS-1$
    } catch (MalformedURLException e) {
      e.printStackTrace();
    }
    return null; 
  }
  public final HashMap<String, Color> getFilterPropertyColorMap() {
    return m_filterPropertyColorMap;
  }
}
