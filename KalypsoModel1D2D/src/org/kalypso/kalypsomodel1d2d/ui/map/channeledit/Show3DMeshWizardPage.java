/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Gernot Belger
 */
class Show3DMeshWizardPage extends WizardPage
{
  private final ChannelEditData m_data;

  public Show3DMeshWizardPage( final String pageName, final ChannelEditData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Mesh Preview" );
    setDescription( "Use mouse to navigate the mesh" );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    setControl( composite );

    // set the layout so our canvas fills the whole control
    composite.setLayout( new FillLayout() );

//    final GLData data = new GLData();
//    data.doubleBuffer = true;
//
//    final GLCanvas canvas = new GLCanvas( composite, SWT.BORDER, data );
//
//    canvas.setCurrent();
//
//    final GLProfile profile = GLProfile.getDefault();
//
//    final GLContext context = GLDrawableFactory.getFactory( profile ).createExternalGLContext();
//
//    canvas.addListener( SWT.Resize, new Listener()
//    {
//      @Override
//      public void handleEvent( final Event event )
//      {
//        final Rectangle bounds = canvas.getBounds();
//        final float fAspect = (float)bounds.width / (float)bounds.height;
//        canvas.setCurrent();
//        context.makeCurrent();
//        final GL gl = context.getGL();
//        gl.glViewport( 0, 0, bounds.width, bounds.height );
//        gl.glMatrixMode( GL.GL_PROJECTION );
//        gl.glLoadIdentity();
//
//        final GLU glu = new GLU();
//        glu.gluPerspective( 45.0f, fAspect, 0.5f, 400.0f );
//
//        gl.glMatrixMode( GL.GL_MODELVIEW );
//        gl.glLoadIdentity();
//        context.release();
//      }
//    } );
//
//    context.makeCurrent();
//    final GL gl = context.getGL();
//    gl.glClearColor( 1.0f, 1.0f, 1.0f, 1.0f );
//    gl.glColor3f( 1.0f, 0.0f, 0.0f );
//    gl.glHint( GL.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST );
//    gl.glClearDepth( 1.0 );
//    gl.glLineWidth( 2 );
//    gl.glEnable( GL.GL_DEPTH_TEST );
//    context.release();
//
//    final Display display = getShell().getDisplay();
//    display.asyncExec( new Runnable()
//    {
//      int rot = 0;
//
//      @Override
//      public void run( )
//      {
//        if( !canvas.isDisposed() )
//        {
//          canvas.setCurrent();
//          context.makeCurrent();
//          final GL gl = context.getGL();
//          gl.glClear( GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT );
//          gl.glClearColor( .3f, .5f, .8f, 1.0f );
//          gl.glLoadIdentity();
//          gl.glTranslatef( 0.0f, 0.0f, -10.0f );
//          final float frot = rot;
//          gl.glRotatef( 0.15f * rot, 2.0f * frot, 10.0f * frot, 1.0f );
//          gl.glRotatef( 0.3f * rot, 3.0f * frot, 1.0f * frot, 1.0f );
//          rot++;
//          gl.glPolygonMode( GL.GL_FRONT_AND_BACK, GL.GL_LINE );
//          gl.glColor3f( 0.9f, 0.9f, 0.9f );
//          drawTorus( gl, 1, 1.9f + ((float)Math.sin( (0.004f * frot) )), 15, 15 );
//          canvas.swapBuffers();
//          context.release();
//          display.asyncExec( this );
//        }
//      }
//    } );
  }

//  static void drawTorus( final GL gl, final float r, final float R, final int nsides, final int rings )
//  {
//    final float ringDelta = 2.0f * (float)Math.PI / rings;
//    final float sideDelta = 2.0f * (float)Math.PI / nsides;
//    float theta = 0.0f, cosTheta = 1.0f, sinTheta = 0.0f;
//    for( int i = rings - 1; i >= 0; i-- )
//    {
//      final float theta1 = theta + ringDelta;
//      final float cosTheta1 = (float)Math.cos( theta1 );
//      final float sinTheta1 = (float)Math.sin( theta1 );
//      gl.glBegin( GL.GL_QUAD_STRIP );
//      float phi = 0.0f;
//      for( int j = nsides; j >= 0; j-- )
//      {
//        phi += sideDelta;
//        final float cosPhi = (float)Math.cos( phi );
//        final float sinPhi = (float)Math.sin( phi );
//        final float dist = R + r * cosPhi;
//        gl.glNormal3f( cosTheta1 * cosPhi, -sinTheta1 * cosPhi, sinPhi );
//        gl.glVertex3f( cosTheta1 * dist, -sinTheta1 * dist, r * sinPhi );
//        gl.glNormal3f( cosTheta * cosPhi, -sinTheta * cosPhi, sinPhi );
//        gl.glVertex3f( cosTheta * dist, -sinTheta * dist, r * sinPhi );
//      }
//      gl.glEnd();
//      theta = theta1;
//      cosTheta = cosTheta1;
//      sinTheta = sinTheta1;
//    }
//  }
}