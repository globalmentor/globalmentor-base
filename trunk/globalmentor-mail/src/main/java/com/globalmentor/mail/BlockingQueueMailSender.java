/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.mail;

import java.util.concurrent.BlockingQueue;

import javax.mail.*;

import com.globalmentor.collections.AbstractRunnableBlockingQueueConsumer;
import com.globalmentor.log.Log;

import static com.globalmentor.java.Objects.*;

/**A consumer that takes mail messages from a blocking queue and sends them.
The given transport connection is opened and closed as needed.
Errors are logged.
This runnable's interruption policy is to end execution.
@author Garret Wilson
*/
public class BlockingQueueMailSender extends AbstractRunnableBlockingQueueConsumer<Message>
{

	/**The transport used for sending messages.*/
	private final Transport transport;

		/**@return The transport used for sending messages.*/
		protected Transport getTransport() {return transport;}

	/**The authentication user for sending the messages.*/
	private final String user;

		/**@return The authentication user for sending the messages.*/
		protected String getUser() {return user;}

	/**The authentication password for sending the messages.*/
	private final String password;

		/**@return The authentication password for sending the messages.*/
		protected String getPassword() {return password;}
		
	/**Blocking queue constructor.
	@param blockingQueue The blocking queue from which elements will be consumed.
	@param transport The tranport to use for sending the messages.
	@param user The authentication user for sending the messages.
	@param password The authentication password for sending the messages.
	@exception NullPointerException if the given blocking queue, transport, user, and/or password is <code>null</code>.
	*/
	public BlockingQueueMailSender(final BlockingQueue<Message> blockingQueue, final Transport transport, final String user, final String password)
	{
		super(blockingQueue);	//construct the parent class
		this.transport=checkInstance(transport, "Transport cannot be null.");
		this.user=checkInstance(user, "User cannot be null.");
		this.password=checkInstance(password, "Password cannot be null.");
	}

	/**Consumes an email message from the queue.
	@param message The email message to consume.
	*/
	public void consume(final Message message)
	{
		final Transport transport=getTransport();	//get the current transport
		try
		{
			if(!transport.isConnected())	//if the transport is not connected
			{
				transport.connect(getUser(), getPassword());	//connect to the transport
			}
			transport.sendMessage(message, message.getAllRecipients());	//send the message to all its recipients
			if(getBlockingQueue().isEmpty())	//if there are currently no more elements in the queue
			{
				transport.close();	//close the transport; we'll open it as needed
			}
		}
		catch(final MessagingException messagingException)	//if there is a messaging error
		{
			Log.info(messagingException);	//log the error
		}
	}

	/**Called when the consumer is stopped after processing ends.
	This version closes the transport.
	*/
	protected void stopped()
	{
		try
		{
			getTransport().close();	//close the transport
			super.stopped();	//do the default stopping
		}
		catch(final MessagingException messagingException)	//if there is a messaging error
		{
			Log.info(messagingException);	//log the error
		}
	}

}
