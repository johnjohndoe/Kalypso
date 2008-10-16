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
package org.kalypso.project.database.server;

import javax.jws.WebService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;
import org.kalypso.project.database.server.model.HibernateUtil;

/**
 * @author kuch
 */
@WebService(endpointInterface = "org.kalypso.project.database.sei.IProjectDatabase")
public class ProjectDatabase implements IProjectDatabase
{
  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#testMethod()
   */
  @Override
  public String testMethod( )
  {
    /* test storing and reading of projects */
    /** Getting the Session Factory and session */
    SessionFactory session = HibernateUtil.getSessionFactory();
    Session sess = session.getCurrentSession();
    /** Starting the Transaction */
    Transaction tx = sess.beginTransaction();
    /** Creating Pojo */
    KalypsoProjectBean project = new KalypsoProjectBean( "TestProject" );

    /** Saving POJO */
    sess.save( project );

    /** Commiting the changes */
    tx.commit();
    System.out.println( "Record Inserted" );
    /** Closing Session */
    session.close();

    return "blub";
  }

  /**
   * @see org.kalypso.project.database.sei.IProjectDatabase#getProjects()
   */
  @Override
  public KalypsoProjectBean[] getProjects( )
  {
    return new KalypsoProjectBean[] { new KalypsoProjectBean( "Test Project" ) };
  }
}
