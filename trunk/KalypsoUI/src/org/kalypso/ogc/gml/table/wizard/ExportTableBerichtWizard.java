package org.kalypso.ogc.gml.table.wizard;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.gml.schema.Mapper;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.wizard.metadata.FeaturePage;
import org.kalypso.util.io.CSV;

/**
 * @author belger
 */
public class ExportTableBerichtWizard extends Wizard
{
  private final DocBean m_docBean;

  private FeaturePage m_featurePage;
  private ExportTableOptionsPage m_optionPage;

  private final Feature m_feature;

  private LayerTableViewer m_layerTable;
  
  public ExportTableBerichtWizard( final LayerTableViewer layerTable, final DocBean doc )
  {
    final IDialogSettings workbenchSettings = UIPlugin.getDefault().getDialogSettings();
    IDialogSettings section = workbenchSettings.getSection( "ExportTableWizard" );//$NON-NLS-1$
    if( section == null )
      section = workbenchSettings.addNewSection( "ExportTableWizard" );//$NON-NLS-1$
    setDialogSettings( section );
    
    setWindowTitle( "Berichtsablage" );

    m_layerTable = layerTable;
    m_docBean = doc;
    
    // create featuretype from bean
    final Collection ftpColl = new ArrayList();
    final Collection fpColl = new ArrayList();
    final Map metadata = doc.getMetadata();
    final int[] ints = new int[metadata.size()];
    int count = 0;
    for( final Iterator iter = metadata.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      
      final String name = entry.getKey().toString();
      final String xmltype = entry.getValue().toString();
      String typename = null;
      try
      {
        typename = Mapper.mapXMLSchemaType2JavaType( xmltype );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        typename = "java.lang.String";
      }
      ftpColl.add( FeatureFactory.createFeatureTypeProperty( name, typename, false ) );
      fpColl.add( FeatureFactory.createFeatureProperty( name, null ) );
      
      ints[count++] = 1;
    }
    
    final FeatureTypeProperty[] ftps = (FeatureTypeProperty[])ftpColl.toArray( new FeatureTypeProperty[ftpColl.size()] );
    final FeatureType ft = FeatureFactory.createFeatureType( "docbean", null, ftps, ints, ints, null, null );
    
    m_feature = FeatureFactory.createFeature( "0", ft, (FeatureProperty[])fpColl.toArray( new FeatureProperty[fpColl.size()] ) );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  public void addPages()
  {
    super.addPages();
    
    m_featurePage = new FeaturePage( "featurePage", "Metadaten editieren", null, true, m_feature );
    m_optionPage = new ExportTableOptionsPage( "optionPage", "Export Otionen", null );
    
    addPage( m_featurePage );
    addPage( m_optionPage );
  }

  public boolean performFinish()
  {
    try
    {
      // aus dem Feature die Bean füllen
      final Collection changes = new ArrayList();
      m_featurePage.collectChanges( changes );

      final Map metadata = m_docBean.getMetadata();
      for( final Iterator iter = changes.iterator(); iter.hasNext(); )
      {
        final FeatureChange fc = (FeatureChange)iter.next();

        final String typename = metadata.get( fc.property ).toString();
        
        final Object newValue = Mapper.mapJavaValueToXml( fc.newValue, typename );
        metadata.put( fc.property, newValue );
      }

      final LayerTableViewer layerTable = m_layerTable;

      // das Dokument erzeugen
      final File destinationFile = new File( m_docBean.getLocation() );
      final String[][] csv = layerTable.exportTable( m_optionPage.getOnlySelected() );
      final PrintWriter pw = new PrintWriter( new FileWriter( destinationFile ) );
      CSV.writeCSV( csv, pw );
      pw.close();
      
      m_optionPage.saveWidgetValues();
    }
    catch( final Exception e )
    {
      // TODO: errormessage
      
      e.printStackTrace();
      
      return false;
    }

    return true;
  }
}