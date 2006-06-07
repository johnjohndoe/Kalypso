package org.kalypso.model.wspm.ui.profil.view.chart.action;

import java.awt.geom.Point2D;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionManager;
import org.eclipse.jface.action.StatusLineLayoutData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;

import de.belger.swtchart.mouse.IChartPosListener;

public class StatusPosContributionItem extends ContributionItem
{
  private static final String POS_FORMAT = "%10.4f  /  %10.4f";

  public final static int DEFAULT_CHAR_WIDTH = 40;

  private final int m_charWidth;

  private final IChartPosListener m_posListener = new IChartPosListener()
  {
    public void onPosChanged( final Point2D logpoint, final boolean inScreen )
    {
      final StringBuffer msg = new StringBuffer( "Pos: " );
      if( inScreen )
        msg.append( String.format( POS_FORMAT, new Object[] { logpoint.getX(), logpoint.getY() } ) );

      setText( msg.toString() );
    }
  };

  private CLabel m_label;

  private String m_text;

  /**
   * The composite into which this contribution item has been placed. This will be <code>null</code> if this instance
   * has not yet been initialized.
   */
  // private Composite m_statusLine = null;
  private int widthHint = -1;

  private int heightHint = -1;

  private ProfilChartView m_chartView;

  public StatusPosContributionItem( final String id )
  {
    this( id, DEFAULT_CHAR_WIDTH );
  }

  public StatusPosContributionItem( final String id, final int charWidth )
  {
    super( id );
    m_charWidth = charWidth;
    setVisible( false ); // no text to start with
  }

  /**
   * @see org.eclipse.jface.action.ContributionItem#dispose()
   */
  @Override
  public void dispose( )
  {
    unhook();

    super.dispose();
  }

  private void unhook( )
  {
    if( m_chartView != null )
    {
      m_chartView.removeChartPosListener( m_posListener );
      m_chartView = null;
    }
  }

  @Override
  public void fill( final Composite parent )
  {
    final Label sep = new Label( parent, SWT.SEPARATOR );
    m_label = new CLabel( parent, SWT.SHADOW_NONE );

    if( widthHint < 0 )
    {
      GC gc = new GC( parent );
      gc.setFont( parent.getFont() );
      FontMetrics fm = gc.getFontMetrics();
      widthHint = fm.getAverageCharWidth() * m_charWidth;
      heightHint = fm.getHeight();
      gc.dispose();
    }

    StatusLineLayoutData data = new StatusLineLayoutData();
    data.widthHint = widthHint;
    m_label.setLayoutData( data );
    m_label.setText( m_text );

    data = new StatusLineLayoutData();
    data.heightHint = heightHint;
    sep.setLayoutData( data );

  }

  protected void setText( final String text )
  {
    if( text == null )
      throw new NullPointerException();

    m_text = text;

    if( m_label != null && !m_label.isDisposed() )
      m_label.setText( m_text );

    if( m_text.length() == 0 )
    {
      if( isVisible() )
      {
        setVisible( false );
        final IContributionManager contributionManager = getParent();
        if( contributionManager != null )
          contributionManager.update( true );
      }
    }
    else
    {
      if( !isVisible() )
      {
        setVisible( true );
        final IContributionManager contributionManager = getParent();
        if( contributionManager != null )
          contributionManager.update( true );
      }
    }
  }

  public void setChartView( final ProfilChartView chartView )
  {
    unhook();

    m_chartView = chartView;

    if( m_chartView != null )
      m_chartView.addChartPosListener( m_posListener );
  }

}
