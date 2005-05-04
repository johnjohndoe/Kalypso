package org.kaylpso.ui.wizard.shape;

import java.io.File;
import java.net.URL;
import java.rmi.RemoteException;

import javax.xml.bind.JAXBException;

import org.deegree.services.wms.StyleNotDefinedException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.data.IKalypsoDataImportWizard;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kaylpso.ui.action.AddThemeCommand;

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

/**
 * 
 * @author Kuepferle
 *  
 */
public class ImportShapeSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportShapeFileImportPage m_page;

  private GisMapOutlineViewer m_outlineviewer;

  public ImportShapeSourceWizard()
  {
    super();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {

    try
    {
      File shapeBaseFile = m_page.getShapeBaseFile();
      //read Shapefile
      GMLWorkspace shapeWS = ShapeSerializer.deserialize( shapeBaseFile.toString(),
          m_page.getCRS(), null );

      //get DefaultStyle
      String styleName = shapeBaseFile.getName();
      URL styleHref = KalypsoGisPlugin.getDefault().getDefaultStyleFactory().getDefaultStyle(
          shapeWS.getFeatureType( shapeBaseFile.toString() ), styleName );

      //Add Layer to mapModell
      IMapModell mapModell = m_outlineviewer.getMapModell();
      String themeName = FileUtilities.nameWithoutExtension( m_page.getShapePath().lastSegment() );
      String fileName = m_page.getShapeBaseRelativePath() + "#" + m_page.getCRS().getName();
      AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell, themeName,
          "shape", "featureMember", fileName, "sld", styleName,
          styleHref.toString(), "simple" );
      m_outlineviewer.postCommand( command, null );
    }

    catch( GmlSerializeException e )
    {
      e.printStackTrace();
      // TODO
    }
    catch( StyleNotDefinedException e )
    {
      e.printStackTrace();
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    m_page.removeListeners();
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  //do nothing
  }

  public void addPages()
  {

    m_page = new ImportShapeFileImportPage( "shapefileimport", "ESRI(tm) ein Projekt importieren",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    if( m_outlineviewer != null )
    {
      m_page.setProjectSelection( m_outlineviewer.getMapModell().getProject() );
    }
    addPage( m_page );

  }

  /**
   * 
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
  {
    m_outlineviewer = outlineviewer;
  }

}