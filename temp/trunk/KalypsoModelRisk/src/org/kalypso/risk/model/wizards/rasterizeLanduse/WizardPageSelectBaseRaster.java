package org.kalypso.risk.model.wizards.rasterizeLanduse;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypso.ui.ImageProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class WizardPageSelectBaseRaster extends WizardPage
{
  private Combo m_cmbSelectBaseRaster;

  private Map<String, IWaterdepthCoverage> m_coverageMap;

  private final IWaterdepthCoverageModel m_model;

  public WizardPageSelectBaseRaster( final IWaterdepthCoverageModel model )
  {
    super( "Select base raster", "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ );
    m_model = model;
    setTitle( "Select base raster title" );
    setDescription( "Select base raster description" );
    m_coverageMap = new HashMap<String, IWaterdepthCoverage>();
  }

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *            the parent composite
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    // Waterlevel rasters combo box
    new Label( container, SWT.NONE ).setText( "Select base raster" );
    m_cmbSelectBaseRaster = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd0 = new GridData();
    gd0.horizontalAlignment = GridData.FILL;
    gd0.widthHint = 175;
    m_cmbSelectBaseRaster.setLayoutData( gd0 );
    initContents();
    setControl( container );
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's content based upon the cached selection
   * provided by the wizard.
   * 
   * @throws CoreException
   */
  private void initContents( )
  {
    int maxReturnPeriodIndex = 0;
    int currentIndex = 0;
    int maxReturnPeriod = Integer.MIN_VALUE;
    for( final IWaterdepthCoverage coverage : m_model.getWaterdepthCoverageCollection() )
    {
      if( maxReturnPeriod < coverage.getReturnPeriod() )
      {
        maxReturnPeriod = coverage.getReturnPeriod();
        maxReturnPeriodIndex = currentIndex;
      }
      final String key = coverage.getReturnPeriod() + " year flood";
      m_coverageMap.put( key, coverage );
      m_cmbSelectBaseRaster.add( key );
      currentIndex++;
    }
    m_cmbSelectBaseRaster.select( maxReturnPeriodIndex );
  }

  public IWaterdepthCoverage getSelectedCoverage( )
  {
    return m_coverageMap.get( m_cmbSelectBaseRaster.getText() );
  }

}
