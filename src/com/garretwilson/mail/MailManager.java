package com.garretwilson.mail;

import java.util.Properties;
import java.util.concurrent.*;

import javax.mail.*;

import static com.garretwilson.mail.MailConstants.*;

/**A class for managing mail with asynchronous sending.
@author Garret Wilson
*/
public class MailManager
{

	/**The session used for sending mail.*/
	private final Session session;

		/**@return The session used for sending mail.*/
		public Session getSession() {return session;}

	/**The queue for sending mail.*/
	private final BlockingQueue<Message> sendQueue;

		/**@return The queue for sending mail.*/
		public BlockingQueue<Message> getSendQueue() {return sendQueue;}

	/**The consumer that sends mail placed in the queue.*/
	private final BlockingQueueMailSender mailSender;

		/**@return The consumer that sends mail placed in the queue.*/
		protected BlockingQueueMailSender getMailSender() {return mailSender;}

	/**The thread for asynchronous sending.*/
	private final Thread sendThread;

		/**The thread for asynchronous sending.*/
		public Thread getSendThread() {return sendThread;}

	/**Properties constructor.
	The transport protocol should be specified in the {@value MailConstants#MAIL_TRANSPORT_PROTOCOL_PROPERTY} property.	
	Besides the default JavaMail properties, this implementation expects the mail authentication user to be specified in the <code>mail.<var>transport</var>.user</code> property
	and the mail authentication password to be specified in the <code>mail.<var>transport</var>.password</code> property
	@param properties The JavaMail properties used to initialize the mail session.
	@exception NullPointerException if the given properties is <code>null</code>.
	@exception NoSuchProviderException if the provider specified in the properties cannot be found.
	*/
	public MailManager(final Properties properties) throws NoSuchProviderException
	{
		session=Session.getInstance(properties);  //create a new mail session
		final String transportProtocol=properties.getProperty(MAIL_TRANSPORT_PROTOCOL_PROPERTY);	//get the transport protocol
		final String user=properties.getProperty(MAIL_PROTOCOL_USER_PROPERTY_TEMPLATE.apply(transportProtocol));	//get the user for the transport protocol
		final String password=properties.getProperty(MAIL_PROTOCOL_PASSWORD_PROPERTY_TEMPLATE.apply(transportProtocol));	//get the password for the transport protocol
		sendQueue=new LinkedBlockingQueue<Message>();	//create a new queue for sending mail messages
		mailSender=new BlockingQueueMailSender(sendQueue, session.getTransport(), user, password);	//create the mail sender
		sendThread=new Thread(mailSender, getClass().getName());	//create a new send thread
		sendThread.setDaemon(true);	//make the sendn thread a daemon so that it won't hold up the application when the system shuts down
		sendThread.start();	//start the send thread
	}
}
