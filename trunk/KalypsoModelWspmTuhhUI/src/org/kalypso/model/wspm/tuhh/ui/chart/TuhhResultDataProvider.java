package org.kalypso.model.wspm.tuhh.ui.chart;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.ui.view.chart.layer.IWspLayerData;

/**
 * @author kimwerner
 */
public final class TuhhResultDataProvider implements IWspLayerData
{
  private final IProfil m_profil;

  private final IWspmResultNode m_results;

  private final Set<IWspmResult> m_activeNames = new LinkedHashSet<IWspmResult>();

  public TuhhResultDataProvider( final IProfil profil, IWspmResultNode results )
  {
    m_profil = profil;
    m_results = results;
  }

  @Override
  public double searchValue( Object name, BigDecimal station ) throws Exception
  {
    final IProfilPointMarker[] marker = m_profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    return Math.max( ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, marker[0].getPoint() ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, marker[1].getPoint() ) );
  }

  @Override
  public IWspmResult[] getNames( ) throws Exception
  {
    return findResults( m_results );
  }

  private static IWspmResult[] findResults( IWspmResultNode node )
  {
    final Collection<IWspmResult> results = new ArrayList<IWspmResult>();

    if( node instanceof IWspmResult )
      results.add( (IWspmResult) node );

    final IWspmResultNode[] childNodes = node.getChildResults();
    for( final IWspmResultNode child : childNodes )
    {
      final IWspmResult[] childResults = findResults( child );
      results.addAll( java.util.Arrays.asList( childResults ) );
    }

    return results.toArray( new IWspmResult[results.size()] );
  }

  @Override
  public String[] getActiveNames( ) throws Exception
  {
    return m_activeNames.toArray( new String[m_activeNames.size()] );
  }

  @Override
  public void activateNames( Object[] names ) throws Exception
  {
    IWspmResult[] results = Arrays.castArray( names, new IWspmResult[names.length] );
    m_activeNames.addAll( java.util.Arrays.asList( results ) );
  }
}