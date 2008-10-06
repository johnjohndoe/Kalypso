package org.kalypso.ui.wizard.shape;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;

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
 * @author Kuepferle
 */
public class ImportShapeSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportShapeFileImportPage m_page;

  private ICommandTarget m_outlineviewer;

  private IKalypsoLayerModell m_modell;

  public ImportShapeSourceWizard( )
  {
    super();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // Add Layer to mapModell
    final IKalypsoLayerModell mapModell = m_modell;
    final String themeName = FileUtilities.nameWithoutExtension( m_page.getShapePath().lastSegment() );
    final String fileName = m_page.getShapeBaseRelativePath() + "#" + m_page.getCRS();

    final IPath stylePath = m_page.getStylePath();
    final String styleLocation = stylePath == null ? null : stylePath.toString();
    final String styleName = m_page.getStyleName();

    final AddThemeCommand command = new AddThemeCommand( mapModell, themeName, "shape", ShapeSerializer.PROPERTY_FEATURE_MEMBER.getLocalPart(), fileName, "sld", styleName, styleLocation, "simple" );
    m_outlineviewer.postCommand( command, null );

    m_page.removeListeners();
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // do nothing
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportShapeFileImportPage( "shapefileimport", "ESRI(tm) ein Projekt importieren", ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    if( m_outlineviewer != null )
    {
      m_page.setProjectSelection( m_modell.getProject() );
    }
    addPage( m_page );

  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_outlineviewer = commandTarget;
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.IKalypsoLayerModell)
   */
  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

}