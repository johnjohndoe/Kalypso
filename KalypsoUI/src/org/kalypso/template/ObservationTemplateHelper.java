package org.kalypso.template;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.template.LinkedDiagramCurve;
import org.kalypso.ogc.sensor.template.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.template.LinkedTableViewTemplate;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.util.xml.xlink.JAXBXLink;

/**
 * @author schlienger
 */
public class ObservationTemplateHelper
{
  private final static org.kalypso.template.obsdiagview.ObjectFactory m_obsDiagFactory = new org.kalypso.template.obsdiagview.ObjectFactory();
  private final static org.kalypso.template.obstableview.ObjectFactory m_obsTableFactory = new org.kalypso.template.obstableview.ObjectFactory();

  private ObservationTemplateHelper()
  {
  // not to be instanciated
  }

  /**
   * Loads a LinkedDiagramTemplate from the given file.
   */
  public static IDiagramTemplate loadDiagramTemplate( final IFile file ) throws CoreException,
      JAXBException, IOException
  {
    final InputStream ins = file.getContents();
    final ObsdiagviewType baseTemplate = (ObsdiagviewType)m_obsDiagFactory.createUnmarshaller().unmarshal(
        ins );
    ins.close();

    return new LinkedDiagramTemplate( baseTemplate, file.getProject() );
  }

  /**
   * Creates a LinkedDiagramCurve for the given binding object.
   */
  public static LinkedDiagramCurve createCurve( final ObsdiagviewType.CurveType baseCurve,
      final IDiagramTemplate template )
  {
    Properties mappings = new Properties();

    for( Iterator it = baseCurve.getMapping().iterator(); it.hasNext(); )
    {
      TypeAxisMapping am = (TypeAxisMapping)it.next();

      mappings.setProperty( am.getObservationAxis(), am.getDiagramAxis() );
    }

    return new LinkedDiagramCurve( baseCurve.getLinktype(), new JAXBXLink( baseCurve ), baseCurve
        .getName(), mappings, template );
  }

  /**
   * 
   */
  public static LinkedTableViewTemplate loadTableViewTemplate( final IFile file ) throws CoreException, JAXBException, IOException
  {
    final InputStream ins = file.getContents();
    final ObstableviewType baseTemplate = (ObstableviewType)m_obsTableFactory.createUnmarshaller().unmarshal( ins );
    ins.close();
    
    return new LinkedTableViewTemplate( baseTemplate, file.getProject() );
  }
}