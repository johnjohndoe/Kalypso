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
package org.kalypso.ui.calcwizard.bericht;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.kalypso.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.java.util.Arrays;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.metadoc.ExportBerichtWizard;
import org.kalypso.ui.metadoc.IExportableDocument;
import org.kalypso.ui.wizard.feature.ChooseFeaturePage;

/**
 * @author belger
 */
public class ExportWizardBerichtWizard extends ExportBerichtWizard
{
  private final FeatureList m_features;
  private final String m_nameProperty;
  private ChooseFeaturePage m_chooseFeaturePage;
  private final List m_checkedFeatures;
  private Feature[] m_choosenFeatures;
  private IBerichtExporter[] m_choosenExporter;
  private final IBerichtExporter[] m_exporters;
  private ArrayChooserPage m_chooseFromListPage;
  

  public ExportWizardBerichtWizard( final FeatureList features, final List checkedFeatures, final String nameProperty, final IExportableDocument document2export, final DocBean doc, final IBerichtExporter[] exporters )
  {
    super( document2export, doc );
    
    m_features = features;
    m_checkedFeatures = checkedFeatures;
    m_nameProperty = nameProperty;
    m_exporters = exporters;
  }
  
  /**
   * @see org.kalypso.ui.metadoc.ExportBerichtWizard#addPages()
   */
  public void addPages()
  {
    m_chooseFeaturePage = new ChooseFeaturePage( m_features, null, m_checkedFeatures.toArray(), m_nameProperty, "chooseFeatures", "Für diese Vorhersagepegel werden die Berichte erzeugt:", ImageProvider.IMAGE_UTIL_BERICHT_WIZ );
    m_chooseFromListPage = new ArrayChooserPage( m_exporters, "arrayChooser", "Diese Berichtsarten werden erzeugt:", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); 
    
    addPage( m_chooseFeaturePage );
    addPage( m_chooseFromListPage );
    
    super.addPages();
  }
  
  /**
   * @see org.kalypso.ui.metadoc.ExportBerichtWizard#performFinish()
   */
  public boolean performFinish()
  {
    // don't call super.performFinish()
    // instead:
    commitData();
    
    m_choosenFeatures = m_chooseFeaturePage.getSelected();
    m_choosenExporter = (IBerichtExporter[])Arrays.castArray( m_chooseFromListPage.getChoosen(), new IBerichtExporter[0] );
    
    return true;
  }
  
  public Feature[] getChoosenFeatures()
  {
    return m_choosenFeatures;
  }

  public IBerichtExporter[] getChoosenExporter()
  {
    return m_choosenExporter;
  }
}
